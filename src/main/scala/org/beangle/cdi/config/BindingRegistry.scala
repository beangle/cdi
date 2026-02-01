/*
 * Copyright (C) 2005, The Beangle Software.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.beangle.cdi.config

import org.beangle.commons.cdi.Binder.*
import org.beangle.commons.cdi.Reconfig.ReconfigType
import org.beangle.commons.cdi.{Binder, Condition, Reconfig, nowire}
import org.beangle.commons.collection.Collections
import org.beangle.commons.lang.reflect.{BeanInfo, BeanInfos, TypeInfo}
import org.beangle.commons.lang.time.Stopwatch
import org.beangle.commons.logging.Logging

import scala.collection.mutable

class BindingRegistry(background: collection.Map[String, Class[_]]) extends Binder.Registry, Logging {
  private val beans = new mutable.HashMap[String, Binder.RegistryItem]
  private val namesByType = new collection.mutable.HashMap[Class[_], List[String]]
  private val typesByName = new mutable.HashMap[String, Class[_]]
  private val primaries = new mutable.HashMap[Class[_], String]

  typesByName.addAll(background)

  override def register(items: Iterable[Binder.RegistryItem]): Unit = {
    val watch = new Stopwatch(true)
    rebuildCache()
    val defns = items.toBuffer
    val mandatories = defns.filter(_.condition == Condition.None)
    defns.subtractAll(mandatories)
    addBeans(mandatories)
    var meeted = defns.filter(i => i.condition.meet(this))
    while (meeted.nonEmpty) {
      addBeans(meeted)
      defns.subtractAll(meeted)
      meeted = defns.filter(i => i.condition.meet(this))
    }
    logger.info(s"Auto register ${items.size} beans in $watch")
  }

  def allBeans: Iterable[Binder.RegistryItem] = {
    beans.values
  }

  /** Apply reconfig to bean definitions
   *
   * @param registry
   * @param bindRegistry
   */
  def reconfig(reconfigs: Iterable[Reconfig]): Unit = {
    val watch = Stopwatch.start()
    val updated = new mutable.HashSet[String]
    val removed = new mutable.HashSet[String]
    reconfigs foreach { reconfig =>
      reconfig.definitions.values foreach { rd =>
        val beanName = rd.beanName
        rd.configType match {
          case ReconfigType.Remove =>
            beans.remove(beanName) match {
              case Some(old) => removed.addOne(beanName)
              case None =>
                if reconfig.ignoreMissing then logger.warn(s"No bean $beanName to remove")
                else throw new RuntimeException(s"Without bean $beanName to remove")
            }
          case ReconfigType.Update =>
            beans.get(beanName) match {
              case Some(bd) =>
                rd.primaryOf foreach { clz => setPrimary(beanName, clz) }
                val defn = bd match {
                  case sh: Singleton =>
                    beans.remove(beanName)
                    new Definition(beanName, null, null)
                  case d: Definition => d
                }
                defn.merge(rd)
              case None =>
                if reconfig.ignoreMissing then logger.warn(s"No bean $beanName to reconfig")
                else throw new RuntimeException(s"Without bean $beanName to reconfig")
            }
        }
      }
    }
    if (updated.nonEmpty || removed.nonEmpty) {
      logger.debug(s"Reconfig details update $updated and remove $removed")
      logger.info(s"Reconfig complete. update ${updated.size} and remove ${removed.size} in $watch")
    }
  }

  /** Autowire bean by constructor and properties.
   * <ul>policy
   * <li>find unique dependency
   * <li>find primary type of dependency
   * </ul>
   */
  def autowire(): Unit = {
    val watch = new Stopwatch(true)
    var wired = 0
    beans.values foreach {
      case bd: Binder.Definition =>
        if !bd.isAbstract then {
          autowireBean(bd)
          wired += 1
        }
      case _ =>
    }
    logger.info(s"Autowire ${wired} beans using $watch")
  }

  /** Autowire single bean. */
  private def autowireBean(dfn: Binder.Definition): Unit = {
    val beanName = dfn.beanName
    val clazz = dfn.clazz
    val manifest = BeanInfos.get(clazz)
    //1. inject constructor
    findMatchedConstructor(manifest, dfn) foreach { ctor =>
      val paramSize = ctor.parameters.length
      while (dfn.constructorArgs.length < paramSize) {
        dfn.constructorArgs.addOne(null)
      }
      ctor.parameters.indices foreach { i =>
        val param = ctor.parameters(i)
        val value = dfn.constructorArgs(i) match {
          case null => param.defaultValue.getOrElse(createReference(dfn, param.typeinfo))
          case InjectPlaceHolder => createReference(dfn, param.typeinfo)
          case r@Reference(rf) => if contains(rf) then r else wireError(dfn, s"cannot find bean named $rf")
          case Injection(clz) => createReference(dfn, clz)
          case v: Any => v
        }
        dfn.constructorArgs(i) = value
      }
    }

    // check and convert existed property values
    val newValues = Collections.newMap[String, Any]
    dfn.properties foreach { case (n, v) =>
      v match {
        case Reference(rf) =>
          if !contains(rf) then wireError(dfn, s"cannot find bean named $rf")
        case Injection(clz) =>
          newValues.put(n, createReference(dfn, clz))
        case vs: Seq[_] =>
          var changed: Boolean = false
          val newSeq = vs.map {
            case r@Reference(rf) => if !contains(rf) then wireError(dfn, s"cannot find bean named $rf") else r
            case Injection(i) =>
              changed = true
              createReference(dfn, i)
            case ref => ref
          }
          if changed then newValues.put(n, newSeq)
        case _ =>
      }
    }
    dfn.properties.addAll(newValues)

    //2. inject properties
    //1) first by name, 2) by primary with type, 3) by xxxx.default
    val properties = unsatisfiedProperties(dfn, manifest)
    for ((propertyName, propertyType) <- properties) {
      //多值类型
      if (propertyType.isIterable) {
        val v = createReference(dfn, propertyType).asInstanceOf[Iterable[_]]
        if (v.nonEmpty) dfn.properties.put(propertyName, v)
      } else {
        val propertyClazz = if propertyType.isOptional then propertyType.args.head.clazz else propertyType.clazz
        val beanNames = getBeanNames(propertyClazz)
        var best: Option[String] = None
        if (beanNames.size == 1) {
          best = beanNames.headOption
        } else if (beanNames.size > 1) {
          // first autowire by name
          best = beanNames.find(n => n == propertyName)
          // second autowire by primary
          if (best.isEmpty) best = beanNames.find(name => isPrimary(name, propertyClazz))
          // third autowire by default
          if (best.isEmpty) best = beanNames.find(n => n.endsWith(".default"))
        }
        best match {
          case Some(n) => dfn.properties.put(propertyName, Reference(n))
          case None =>
            if (dfn.optionals.contains(propertyName)) {
              if (beanNames.isEmpty) logger.debug(s"$beanName's $propertyName cannot found candidate beans.")
              else logger.warn(s"$beanName's $propertyName expected single bean but found ${beanNames.size}:$beanNames")
            } else {
              if (!propertyType.isOptional && dfn.wiredEagerly && !propertyType.clazz.isPrimitive) {
                wireError(dfn, s"cannot find suitable bean for $propertyName(${beanNames.size}:$beanNames)")
              }
            }
        }
      }
    }
  }

  private def createReference(dfn: Definition, clazz: Class[_]): Reference = {
    val beanNames = getBeanNames(clazz)
    if (beanNames.size == 1) {
      Reference(beanNames.head)
    } else if (beanNames.size > 1) {
      beanNames.find { name => isPrimary(name, clazz) } match {
        case Some(name) => Reference(name)
        case None => wireError(dfn, s"find candinates $beanNames of ${clazz.getName}").asInstanceOf[Reference]
      }
    } else {
      wireError(dfn, s"cannot find dependency of type ${clazz.getName}").asInstanceOf[Reference]
    }
  }

  /** Create reference(s) by typeinfo */
  private def createReference(dfn: Definition, typeinfo: TypeInfo): Any = {
    typeinfo match {
      case TypeInfo.GeneralType(clazz, args) => createReference(dfn, if typeinfo.isOptional then args.head.clazz else clazz)
      case TypeInfo.OptionType(elementType) => createReference(dfn, elementType.clazz)
      case it@TypeInfo.IterableType(clazz, argTypes) =>
        if (it.isCollection) {
          val componentType = it.elementType.clazz
          if (componentType == classOf[AnyRef]) List.empty
          else {
            val beans = getBeanNames(componentType) filterNot (n => n == dfn.beanName) map (bn => Reference(bn))
            if (it.isSet) beans.toSet else beans
          }
        } else {
          val kvtype = it.elementType.args
          val keyType = kvtype(0).clazz
          val valueType = kvtype(1).clazz
          if (keyType == classOf[String]) {
            if (valueType == classOf[AnyRef]) Map.empty
            else getBeanNames(valueType).filterNot(n => n == dfn.beanName).map(bn => (bn, Reference(bn))).toMap
          } else {
            Map.empty
          }
        }
    }
  }

  /** Find unsatisfied properties
   * Unsatisfied property is empty value and not primary type and not starts with java.
   */
  private def unsatisfiedProperties(bd: Binder.Definition, beanInfo: BeanInfo): collection.Map[String, TypeInfo] = {
    val clazz = bd.clazz
    val defined = bd.properties
    val nowires = bd.nowires
    val properties = new collection.mutable.HashMap[String, TypeInfo]
    for ((name, m) <- beanInfo.properties) {
      if (m.writable && !nowires.contains(name) && !defined.contains(name)) {
        val method = m.setter.get
        val typeinfo = m.typeinfo
        if (null == method.getAnnotation(classOf[nowire])) {
          if (typeinfo.isIterable) { //多值类型
            properties.put(name, typeinfo)
          } else { //单值类型
            val propertyClazz = if typeinfo.isOptional then typeinfo.args.head.clazz else typeinfo.clazz
            if autowireable(propertyClazz) then properties.put(name, typeinfo)
          }
        }
      }
    }
    properties
  }

  private def autowireable(clazz: Class[_]): Boolean = {
    !clazz.isPrimitive && !clazz.getName.startsWith("java.") && !clazz.getName.startsWith("scala.")
  }

  /** 查找一个和定义匹配的构造函数
   * find only one constructor or constructor with same parameters count
   *
   * @param manifest
   * @param dfn
   * @return
   */
  private def findMatchedConstructor(manifest: BeanInfo, dfn: Binder.Definition): Option[BeanInfo.ConstructorInfo] = {
    val ctors = manifest.ctors
    if (dfn.constructorArgs.isEmpty) {
      if (ctors.length == 1 && ctors.head.parameters.nonEmpty) ctors.headOption else None
    } else {
      val argLength = dfn.constructorArgs.size
      ctors.find(ctor => ctor.parameters.length == argLength)
    }
  }

  private def addBeans(newers: Iterable[Binder.RegistryItem]): Unit = {
    newers foreach { newer =>
      if (typesByName.contains(newer.beanName)) {
        throw new RuntimeException(s"Already register bean ${newer.beanName}")
      }
      val older = beans.getOrElse(newer.beanName, null)
      var registeable = false
      if (null == older || (older.profile == null && newer.profile != null)) {
        registeable = true
      } else {
        logger.warn(s"Ignore exists bean definition ${newer.beanName} in ${newer.module}")
      }
      if (registeable) {
        newer.primaryOf foreach { clz => setPrimary(newer.beanName, clz) }
        beans.put(newer.beanName, newer)
        typesByName.put(newer.beanName, newer.clazz)
      }
    }
  }

  private def wireError(dfn: Binder.Definition, msg: String): Any = {
    throw new RuntimeException(s"Cannot wired bean ${dfn.beanName}.Due to ${msg}")
  }

  private def setPrimary(name: String, clazz: Class[_]): Unit = {
    primaries.put(clazz, name)
  }

  override def isPrimary(name: String, clazz: Class[_]): Boolean = {
    primaries.get(clazz).contains(name)
  }

  override def contains(clazz: Class[_]): Boolean = {
    getBeanNames(clazz).nonEmpty
  }

  private def contains(beanName: String): Boolean = {
    typesByName.contains(beanName)
  }

  /**
   * Get bean name list according given type
   */
  override def getBeanNames(clazz: Class[_]): List[String] = {
    if (namesByType.contains(clazz)) {
      namesByType(clazz)
    } else {
      val names = for ((name, ty) <- typesByName if clazz.isAssignableFrom(ty) && !name.contains('#')) yield name
      val rs = names.toList
      namesByType.put(clazz, rs)
      rs
    }
  }

  private def rebuildCache(): Unit = {
    namesByType.clear()
  }
}
