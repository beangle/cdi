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

package org.beangle.cdi.spring.config

import org.beangle.cdi.bind.*
import org.beangle.cdi.bind.Binding.*
import org.beangle.cdi.bind.Reconfig.ReconfigType
import org.beangle.cdi.spring.beans.{FactoryBeanProxy, ScalaBeanInfoFactory, ScalaEditorRegistrar}
import org.beangle.cdi.spring.context.HierarchicalEventMulticaster
import org.beangle.cdi.{BeanNamesEventMulticaster, ContainerListener, PropertySource, Scope}
import org.beangle.commons.bean.{Disposable, Factory, Initializing}
import org.beangle.commons.collection.Collections
import org.beangle.commons.config.Resources
import org.beangle.commons.io.IOs
import org.beangle.commons.lang.annotation.description
import org.beangle.commons.lang.reflect.*
import org.beangle.commons.lang.reflect.Reflections.{getGenericParamTypes, newInstance}
import org.beangle.commons.lang.time.Stopwatch
import org.beangle.commons.lang.{ClassLoaders, JVM, Strings, SystemInfo}
import org.beangle.commons.logging.Logging
import org.beangle.commons.net.http.HttpUtils
import org.springframework.beans.factory.FactoryBean
import org.springframework.beans.factory.config.*
import org.springframework.beans.factory.support.*
import org.springframework.core.io.{Resource, UrlResource}

import java.net.URL

/**
 * 完成bean的自动注册和再配置
 *
 * @author chaostone
 */
abstract class BindModuleProcessor extends BeanDefinitionRegistryPostProcessor with Logging {

  var name: String = "default"

  var moduleLocations: Array[Resource] = Array.empty

  var modules: Set[BindModule] = Set.empty

  private var properties = Collections.newMap[String, String]

  private var reconfigs = Collections.newBuffer[Reconfig.Definition]

  var reconfigSetting: ReconfigSetting = null

  /** Automate register and wire bean
   * Reconfig beans
   */
  override def postProcessBeanDefinitionRegistry(bdRegistry: BeanDefinitionRegistry): Unit = {
    // find bean definition by code
    val registry = new SpringBindRegistry(bdRegistry)
    readConfig(bdRegistry)
    val newDefinitions = registerModules(registry)
    //reconfig all bean by spring-config.xml
    reconfig(bdRegistry, registry)
    //register beangle factory
    registerBeangleFactory(bdRegistry, registry)
    //register last one
    registerLast(registry)

    // support initializing/disposable
    lifecycle(registry, bdRegistry)
    // wire by constructor/properties
    autowire(newDefinitions, registry)

    // add container description
    val meType = this.getClass
    registry.getBeanNames(meType) foreach { containerName =>
      val containerDef = bdRegistry.getBeanDefinition(containerName)
      if (null == containerDef.getDescription) {
        containerDef match {
          case abDef: AbstractBeanDefinition => abDef.setDescription(getClassDescription(meType))
          case _ =>
        }
      }
    }
    properties = null
    reconfigs = null
  }

  def postProcessBeanFactory(factory: ConfigurableListableBeanFactory): Unit = {
    factory.registerCustomEditor(classOf[Resources], classOf[ResourcesEditor])
    factory.addPropertyEditorRegistrar(new ScalaEditorRegistrar)
  }

  /** Read spring-config.xml
   */
  private def readConfig(bdRegistry: BeanDefinitionRegistry): Unit = {
    properties ++= SystemInfo.properties
    var profile = properties.get(BindRegistry.ProfileProperty).getOrElse("")
    if (JVM.isDebugMode) profile += ",dev"
    val profiles = Strings.split(profile, ",").map(s => s.trim).toSet

    //collect bindmodules and read reconfig properties
    val moduleSet = new collection.mutable.HashSet[BindModule]
    val effectiveLocations = Collections.newBuffer[Resource]
    moduleLocations foreach { r =>
      val is = r.getInputStream
      (scala.xml.XML.load(is) \ "container") foreach { con =>
        var containerName = (con \ "@name").text
        if (Strings.isEmpty(containerName)) containerName = "default"
        if (containerName == this.name) {
          effectiveLocations += r
          (con \ "module") foreach { moduleElem =>
            val module = loadModule((moduleElem \ "@class").text)
            val anno = module.getClass.getAnnotation(classOf[profile])
            if (null == anno || null != anno && new ProfileMatcher(anno.value).matches(profiles)) {
              module match {
                case ps: PropertySource =>
                  this.properties ++= ps.properties
                case rc: ReconfigModule =>
                  val recfg = new Reconfig
                  rc.configure(recfg)
                  reconfigs ++= recfg.definitions.values
                  this.properties ++= recfg.properties
                case m: BindModule =>
              }
              module match {
                case module1: BindModule => moduleSet += module1
                case _ =>
              }
            }
          }
        }
      }
      IOs.close(is)
    }

    if (null != reconfigSetting) {
      val url = reconfigSetting.url
      if (Strings.isNotBlank(url) && url.startsWith("http")) {
        if (!HttpUtils.access(new URL(url)).isOk) reconfigSetting = null
      }
    }

    if (null != reconfigSetting && Strings.isNotBlank(reconfigSetting.url)) {
      val reader = ReconfigReader
      val watch = new Stopwatch(true)
      val holders = reader.load(new UrlResource(reconfigSetting.url))
      for (holder <- holders) {
        val beanName = holder.name
        if (beanName == "properties") {
          holder.definition.properties foreach { case (k, v) =>
            this.properties += (k -> v.toString)
          }
        } else {
          reconfigs += holder
        }
      }
      logger.info(s"Read ${reconfigSetting.url} in $watch")
    }

    this.moduleLocations = effectiveLocations.toArray
    this.modules = moduleSet.toSet
  }

  private def loadModule(name: String): Any = {
    var moduleClass = ClassLoaders.load(name)
    if (!classOf[BindModule].isAssignableFrom(moduleClass) && !classOf[ReconfigModule].isAssignableFrom(moduleClass)) {
      ClassLoaders.get(name + "$") match {
        case Some(clazz) => moduleClass = clazz
        case None => throw new RuntimeException(name + " is not a module")
      }
    }
    if (moduleClass.getConstructors.length > 0) {
      newInstance(moduleClass)
    } else {
      moduleClass.getDeclaredField("MODULE$").get(null)
    }
  }

  private def reconfig(registry: BeanDefinitionRegistry, bindRegistry: BindRegistry): Unit = {
    val watch = new Stopwatch(true)
    val beanNames = new collection.mutable.HashSet[String]
    reconfigs foreach { rd =>
      val beanName = rd.name
      rd.configType match {
        case ReconfigType.Remove => registry.removeBeanDefinition(beanName)
        case ReconfigType.Update | ReconfigType.Primary =>
          if (rd.configType == ReconfigType.Primary) {
            if (setPrimary(beanName, rd.definition.clazz, registry, bindRegistry)) {
              rd.definition.clazz = null
            }
          }
          if (registry.containsBeanDefinition(beanName)) {
            val successName = mergeDefinition(registry.getBeanDefinition(beanName), rd)
            if (null != successName) beanNames += successName
          } else {
            logger.warn(s"No bean $beanName to reconfig")
          }
      }
    }
    if (beanNames.nonEmpty) logger.info(s"Reconfig $beanNames in $watch")
  }

  private def setPrimary(name: String, interface: Class[_], r: BeanDefinitionRegistry, br: BindRegistry): Boolean = {
    if (interface.isInterface) {
      val names = br.getBeanNames(interface)
      if (names.contains(name)) {
        for (name <- names) br.setPrimary(name, name == name, r.getBeanDefinition(name))
        return true
      }
    }
    false
  }

  /**
   * Try find bean implements factory interface,and convert to spring FactoryBean[_]
   */
  private def registerBeangleFactory(definitionRegistry: BeanDefinitionRegistry, registry: BindRegistry): Unit = {
    for (name <- definitionRegistry.getBeanDefinitionNames if !(name.startsWith("&"))) {
      val defn = definitionRegistry.getBeanDefinition(name).asInstanceOf[AbstractBeanDefinition]
      if (!defn.isAbstract) {
        val clazz = SpringBindRegistry.getBeanClass(definitionRegistry, name)
        // convert factory to spring factorybean
        if (classOf[Factory[_]].isAssignableFrom(clazz) && !classOf[FactoryBean[_]].isAssignableFrom(clazz)) {
          val proxy = new GenericBeanDefinition()
          proxy.setBeanClass(classOf[FactoryBeanProxy[_]])
          proxy.setAutowireMode(AbstractBeanDefinition.AUTOWIRE_NO)
          proxy.setScope(defn.getScope)
          proxy.setPrimary(defn.isPrimary)
          registry.register(name + "#proxy", clazz, defn)
          val objectTypes = getGenericParamTypes(clazz, classOf[Factory[_]])
          if (objectTypes.isEmpty) throw new RuntimeException(s"Cannot find factory object type of class ${clazz.getName}")
          val objectType = objectTypes.values.head
          proxy.getPropertyValues.add("target", new RuntimeBeanReference(name + "#proxy"))
          proxy.getPropertyValues.add("objectType", objectType)
          val description = getClassDescription(clazz)
          proxy.setDescription(if (null != description) description + "的Spring代理" else null)
          registry.register(name, objectType, proxy)
          registry.register("&" + name, classOf[FactoryBeanProxy[_]])
        }
      }
    }
  }

  /**
   * lifecycle.
   */
  private def lifecycle(registry: BindRegistry, definitionRegistry: BeanDefinitionRegistry): Unit = {
    registry.beanNames foreach { name =>
      val clazz = registry.getBeanType(name)
      val springName = if (name.startsWith("&")) name.substring(1) else name
      if (definitionRegistry.containsBeanDefinition(springName)) {
        val defn = definitionRegistry.getBeanDefinition(springName).asInstanceOf[AbstractBeanDefinition]
        // convert Initializing to init-method
        if (classOf[Initializing].isAssignableFrom(clazz) && null == defn.getInitMethodName
          && !defn.getPropertyValues.contains("init-method")) {
          defn.setInitMethodName("init")
        }
        // convert Disposable to destry-method
        if (classOf[Disposable].isAssignableFrom(clazz) && null == defn.getDestroyMethodName
          && !defn.getPropertyValues.contains("destroy-method")) {
          defn.setDestroyMethodName("destroy")
        }
      }
    }
  }

  /** register last buildin beans.
   */
  private def registerLast(registry: BindRegistry): Unit = {
    val eventMulticaster = new Definition("EventMulticaster.default" + System.currentTimeMillis(),
      classOf[HierarchicalEventMulticaster], Scope.Singleton.name)
    eventMulticaster.description = getClassDescription(classOf[BeanNamesEventMulticaster])
    eventMulticaster.property("container", this)
    registry.getBeanNames(classOf[HierarchicalEventMulticaster]).foreach { parentName =>
      eventMulticaster.property("parent", new RuntimeBeanReference(parentName))
      eventMulticaster.primary = true
    }
    registerBean(eventMulticaster, registry)
  }

  /** 合并bean定义
   */
  private def mergeDefinition(target: BeanDefinition, source: Reconfig.Definition): String = {
    if (null == target.getBeanClassName) {
      logger.warn(s"ingore bean definition ${source.name} for without class")
      return null
    }
    val sourceDefn = source.definition
    // 当类型变化后,删除原有配置
    if (null != sourceDefn.clazz && sourceDefn.clazz.getName != target.getBeanClassName) {
      target.setBeanClassName(sourceDefn.clazz.getName)
      target.asInstanceOf[GenericBeanDefinition].setDescription(getClassDescription(sourceDefn.clazz))
      for (pv <- target.getPropertyValues.getPropertyValues) {
        target.getPropertyValues.removePropertyValue(pv)
      }
    }
    sourceDefn.properties foreach { case (p, v) =>
      if (p.startsWith("+")) {
        val prop = p.substring(1)
        val converted = ExtBeanDefinition.convert(v, properties, true)
        target.getPropertyValues.addPropertyValue(prop, converted)
      } else if (p.startsWith("-")) {
        target.getPropertyValues.removePropertyValue(p.substring(1))
      } else {
        val converted = ExtBeanDefinition.convert(v, properties, false)
        target.getPropertyValues.addPropertyValue(p, converted)
      }
    }
    if (null != sourceDefn.constructorArgs && sourceDefn.constructorArgs.nonEmpty) {
      val cav = target.getConstructorArgumentValues
      cav.clear()
      sourceDefn.constructorArgs.foreach(arg => cav.addGenericArgumentValue(ExtBeanDefinition.convert(arg, properties)))
    }
    logger.debug(s"Reconfig bean ${source.name} ")
    source.name
  }

  /** registerModules.
   */
  private def registerModules(registry: BindRegistry): Map[String, ExtBeanDefinition] = {
    val watch = new Stopwatch(true)
    val definitions = new collection.mutable.HashMap[String, Definition]
    val singletons = new collection.mutable.HashMap[String, AnyRef]
    val bean2profiles = new collection.mutable.HashMap[String, profile]

    modules foreach { module =>
      logger.info(s"Binding ${module.getClass.getName}")
      val binder = new Binding(module.getClass.getName)
      module.configure(binder)
      val profile = module.getClass.getAnnotation(classOf[profile])

      binder.singletons foreach { e =>
        val beanName = e._1
        if (singletons.contains(beanName)) {
          if (null != profile && !bean2profiles.contains(beanName)) {
            singletons.put(beanName, e._2)
            bean2profiles.put(beanName, profile)
          } else {
            logger.warn(s"Ignore exists bean definition $beanName in ${module.getClass.getName}")
          }
        } else {
          singletons.put(beanName, e._2)
          bean2profiles.put(beanName, profile)
        }
      }

      for (definition <- binder.definitions) {
        val beanName = definition.beanName
        if (definitions.contains(beanName)) {
          if (null != profile && !bean2profiles.contains(beanName)) {
            definitions.put(beanName, definition)
            bean2profiles.put(beanName, profile)
          } else {
            logger.warn(s"Ignore exists bean definition $beanName in ${module.getClass.getName}")
          }
        } else {
          definitions.put(beanName, definition)
          if (null != profile) bean2profiles.put(beanName, profile)
        }
      }
    }

    var newBeanCount = 0
    singletons foreach {
      case (beanName, singleton) =>
        if (registry.contains(beanName)) logger.warn(s"Ignore exists bean definition $beanName")
        else {
          registry.register(beanName, singleton)
          newBeanCount += 1
        }
    }
    val beanDefinitions = new collection.mutable.HashMap[String, ExtBeanDefinition]
    definitions foreach {
      case (beanName, definition) =>
        if (registry.contains(beanName)) logger.warn(s"Ignore exists bean definition $beanName")
        else {
          beanDefinitions.put(beanName, registerBean(definition, registry))
          newBeanCount += 1
        }
    }
    logger.info(s"Auto register $newBeanCount beans in $watch")
    beanDefinitions.toMap
  }

  /**
   * registerBean.
   */
  private def registerBean(defn: Definition, registry: BindRegistry): ExtBeanDefinition = {
    val bd = new ExtBeanDefinition(defn, properties)
    //register spring factory bean
    if (classOf[FactoryBean[_]].isAssignableFrom(defn.clazz)) {
      var target = defn.targetClass
      if (null == target && !defn.isAbstract) {
        target = newInstance(defn.clazz.asInstanceOf[Class[FactoryBean[_]]]).getObjectType
      }
      registry.register(defn.beanName, target, bd)
      // register concrete factory bean
      if (!defn.isAbstract) registry.register("&" + defn.beanName, defn.clazz)
    } else {
      registry.register(defn.beanName, defn.clazz, bd)
    }
    logger.debug(s"Register definition ${defn.beanName} for ${defn.clazz}")
    bd
  }

  /** Autowire bean by constructor and properties.
   *
   * <ul>policy
   * <li>find unique dependency
   * <li>find primary type of dependency
   * </ul>
   */
  private def autowire(newBeanDefinitions: Map[String, ExtBeanDefinition], registry: BindRegistry): Unit = {
    val watch = new Stopwatch(true)
    for ((name, bd) <- newBeanDefinitions) autowireBean(name, bd, registry)
    logger.info(s"Autowire ${newBeanDefinitions.size} beans using $watch")
  }

  /**
   * convert typeinfo into ReferenceValue
   */
  private def convertInjectValue(typeinfo: TypeInfo, registry: BindRegistry, excluded: String): Any = {
    val result = typeinfo match {
      case TypeInfo.GeneralType(clazz, args) =>
        if typeinfo.isOptional then Injection(args.head.clazz) else Injection(clazz)
      case TypeInfo.OptionType(elementType) => Injection(elementType.clazz)
      case it@TypeInfo.IterableType(clazz, argTypes) =>
        if (it.isCollection) {
          val componentType = it.elementType.clazz
          if (componentType == classOf[AnyRef]) List.empty
          else {
            val beans = registry.getBeanNames(componentType) filterNot (n => n == excluded) map (bn => new RuntimeBeanReference(bn))
            if (it.isSet) beans.toSet else beans.toList
          }
        } else {
          val kvtype = it.elementType.args
          val keyType = kvtype(0).clazz
          val valueType = kvtype(1).clazz
          if (keyType == classOf[String]) {
            if (valueType == classOf[AnyRef]) Map.empty
            else registry.getBeanNames(valueType).filterNot(n => n == excluded).map(bn => (bn, new RuntimeBeanReference(bn))).toMap
          } else {
            Map.empty
          }
        }
    }
    ExtBeanDefinition.convert(result, properties)
  }

  /**
   * autowire single bean.
   */
  private def autowireBean(beanName: String, mbd: ExtBeanDefinition, registry: BindRegistry): Unit = {
    val clazz = SpringBindRegistry.getBeanClass(mbd)
    val manifest = BeanInfos.get(clazz)
    //1. inject constructor
    // find only one constructor or constructor with same parameters count
    val ctor: BeanInfo.ConstructorInfo = {
      val ctors = manifest.ctors
      if (mbd.getConstructorArgumentValues.isEmpty) {
        if (ctors.length == 1) ctors.head else null
      } else {
        val argLength = mbd.getConstructorArgumentValues.getArgumentCount
        ctors.find(ctor => ctor.parameters.length == argLength).orNull
      }
    }

    if (null != ctor && mbd.isInstanceOf[GenericBeanDefinition]) {
      // doesn't have arguments
      if (mbd.getConstructorArgumentValues.isEmpty) {
        val cav = mbd.getConstructorArgumentValues
        ctor.parameters.indices foreach { i =>
          val param = ctor.parameters(i)
          param.defaultValue match {
            case Some(v) => cav.addGenericArgumentValue(v)
            case None => cav.addGenericArgumentValue(convertInjectValue(param.typeinfo, registry, beanName))
          }
        }
      } else {
        // check have inject place holder
        val cav = new ConstructorArgumentValues
        var i = 0
        val params = ctor.parameters
        val itor = mbd.getConstructorArgumentValues.getGenericArgumentValues.iterator
        while (itor.hasNext) {
          val v = itor.next.getValue
          cav.addGenericArgumentValue(
            v match {
              case InjectPlaceHolder => if (i < params.length) convertInjectValue(params(i).typeinfo, registry, beanName) else null
              case _ => v
            })
          i += 1
        }
        mbd.asInstanceOf[GenericBeanDefinition].setConstructorArgumentValues(cav)
      }
    }
    // inject constructor by parameter type
    if (!mbd.getConstructorArgumentValues.isEmpty) {
      val cav = new ConstructorArgumentValues
      val itor = mbd.getConstructorArgumentValues.getGenericArgumentValues.iterator
      while (itor.hasNext) {
        val v = itor.next.getValue
        v match {
          case Injection(argClass) =>
            val beanNames = registry.getBeanNames(argClass)
            if (beanNames.size == 1) {
              cav.addGenericArgumentValue(new RuntimeBeanReference(beanNames(0)))
            } else if (beanNames.size > 1) {
              beanNames.find { name => registry.isPrimary(name) } match {
                case Some(name) => cav.addGenericArgumentValue(new RuntimeBeanReference(name))
                case None => throw new RuntimeException(s"Cannot wire bean ${mbd.getBeanClassName}, find candinates $beanNames of ${argClass.getName}")
              }
            } else {
              throw new RuntimeException(s"Cannot wire bean $mbd.name,cannot find dependency bean of type ${argClass.getName}")
            }
          case _ => cav.addGenericArgumentValue(v)
        }
      }
      mbd.asInstanceOf[GenericBeanDefinition].setConstructorArgumentValues(cav)
    }

    //2. inject properties
    val properties = unsatisfiedNonSimpleProperties(mbd, beanName)
    for ((propertyName, propertyType) <- properties) {
      if (!propertyType.isIterable || propertyType.isOptional) {
        val propertyClazz = if propertyType.isOptional then propertyType.args.head.clazz else propertyType.clazz
        val beanNames = registry.getBeanNames(propertyClazz)
        var binded = false
        if (beanNames.size == 1) {
          mbd.getPropertyValues.add(propertyName, new RuntimeBeanReference(beanNames(0)))
          binded = true
        } else if (beanNames.size > 1) {
          // first autowire by name
          for (name <- beanNames if !binded) {
            if (name.equals(propertyName)) {
              mbd.getPropertyValues.add(propertyName, new RuntimeBeanReference(name))
              binded = true
            }
          }
          // second autowire by primary
          if (!binded) {
            for (name <- beanNames if !binded) {
              if (registry.isPrimary(name)) {
                mbd.getPropertyValues.add(propertyName, new RuntimeBeanReference(name))
                binded = true
              }
            }
          }
          // third autowire by default
          if (!binded) {
            for (name <- beanNames if !binded) {
              if (name.endsWith(".default")) {
                mbd.getPropertyValues.add(propertyName, new RuntimeBeanReference(name))
                binded = true
              }
            }
          }
        }
        if (!binded) {
          if (mbd.optionals.contains(propertyName)) {
            if (beanNames.isEmpty) logger.debug(s"$beanName's $propertyName cannot found candidate beans.")
            else logger.warn(s"$beanName's $propertyName expected single bean but found ${beanNames.size}:$beanNames")
          } else {
            if (!propertyType.isOptional && mbd.wiredEagerly && !clazz.isPrimitive) {
              throw new RuntimeException(s"Cannot find suitable bean for $beanName's $propertyName(${beanNames.size}:$beanNames)")
            }
          }
        }
      } else {
        val v = convertInjectValue(propertyType, registry, beanName)
        v match {
          case jc: java.util.Collection[_] => if (!jc.isEmpty) mbd.getPropertyValues.add(propertyName, jc)
          case jm: java.util.Map[_, _] => if (!jm.isEmpty) mbd.getPropertyValues.add(propertyName, jm)
        }
      }
    }
  }

  /**
   * Find unsatisfied properties<br>
   * Unsatisfied property is empty value and not primary type and not starts with java.
   */
  private def unsatisfiedNonSimpleProperties(mbd: ExtBeanDefinition, beanName: String): collection.Map[String, TypeInfo] = {
    val properties = new collection.mutable.HashMap[String, TypeInfo]
    val bd = mbd.asInstanceOf[GenericBeanDefinition]
    val clazz = SpringBindRegistry.getBeanClass(bd)
    if (!mbd.isAbstract) {
      val pvs = mbd.getPropertyValues
      val nowireProperties = mbd.nowires
      for ((name, m) <- BeanInfos.get(clazz).properties) {
        if (m.writable && !nowireProperties.contains(name)) {
          val method = m.setter.get
          val typeinfo = m.typeinfo
          if (null == method.getAnnotation(classOf[nowire]) && !pvs.contains(name)) {
            if (!typeinfo.isIterable || typeinfo.isOptional) {
              val propertyClazz = if typeinfo.isOptional then typeinfo.args.head.clazz else typeinfo.clazz
              if (!propertyClazz.getName.startsWith("java.") && !propertyClazz.getName.startsWith("scala.")) {
                //Skip Factory.result method for it's a provider,DONOT need wire
                if (!(name == "result" && classOf[Factory[_]].isAssignableFrom(clazz))) properties.put(name, typeinfo)
              }
            } else {
              if (!classOf[ContainerListener].isAssignableFrom(clazz)) properties.put(name, typeinfo)
            }
          }
        }
      }
    }
    properties
  }

  private def autowireable(clazz: Class[_]): Boolean = {
    !clazz.getName.startsWith("java.") && !clazz.getName.startsWith("scala.")
  }

  private def getClassDescription(clazz: Class[_]): String = {
    val containerDescription = clazz.getAnnotation(classOf[description])
    if (null == containerDescription) null else containerDescription.value()
  }
}
