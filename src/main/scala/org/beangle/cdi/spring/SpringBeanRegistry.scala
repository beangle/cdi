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

package org.beangle.cdi.spring

import org.beangle.commons.bean.Factory
import org.beangle.commons.cdi.Binder
import org.beangle.commons.cdi.Binder.Definition
import org.beangle.commons.collection.Collections
import org.beangle.commons.config.Enviroment
import org.beangle.commons.lang.ClassLoaders
import org.beangle.commons.lang.reflect.Reflections.*
import org.springframework.beans.factory.FactoryBean
import org.springframework.beans.factory.config.{BeanDefinition, BeanDefinitionHolder, RuntimeBeanReference, SingletonBeanRegistry}
import org.springframework.beans.factory.support.{AbstractBeanDefinition, BeanDefinitionRegistry, GenericBeanDefinition}

import scala.collection.mutable

/** Discovers bean names and types from Spring registry and registers bind definitions.
 */
object SpringBeanRegistry {

  /** Register bean definitions or singletons to the Spring registry.
   *
   * @param dfns     registry items (definitions or singletons) to register
   * @param registry Spring bean definition registry
   */
  def register(dfns: Iterable[Binder.RegistryItem], registry: BeanDefinitionRegistry): Unit = {
    val singletonRegistry = registry.asInstanceOf[SingletonBeanRegistry]
    dfns foreach {
      case stn: Binder.Singleton => singletonRegistry.registerSingleton(stn.beanName, stn.singleton)
      case dfn: Binder.Definition => registerBean(dfn, registry)
    }
  }

  /** Find all bean names and their resolved types from the registry.
   *
   * @param registry Spring bean definition registry
   * @return map of bean name to bean class
   */
  def findBeans(registry: BeanDefinitionRegistry): mutable.Map[String, Class[_]] = {
    val nameTypes = Collections.newMap[String, Class[_]]
    val singletonRegistry = registry.asInstanceOf[SingletonBeanRegistry]
    singletonRegistry.getSingletonNames foreach { singtonName =>
      nameTypes.put(singtonName, singletonRegistry.getSingleton(singtonName).getClass)
    }

    for (name <- registry.getBeanDefinitionNames) {
      val bd = registry.getBeanDefinition(name)
      val beanClass = if (bd.isAbstract) null else getBeanClass(registry, name)
      if (null != beanClass) {
        try {
          if (classOf[FactoryBean[_]].isAssignableFrom(beanClass)) {
            var objectClass: Class[_] = null
            val objectTypePV = bd.getPropertyValues.getPropertyValue("objectType")
            if (null != objectTypePV) {
              objectClass = objectTypePV.getValue match {
                case clazz: Class[_] => clazz
                case className: String => ClassLoaders.load(className)
              }
            } else {
              objectClass = bd.getPropertyValues.getPropertyValue("target") match {
                case null =>
                  try {
                    newInstance(beanClass.asInstanceOf[Class[FactoryBean[_]]]).getObjectType
                  } catch {
                    case e: Throwable => null
                  }
                case pv =>
                  pv.getValue match {
                    case bdh: BeanDefinitionHolder => ClassLoaders.load(bdh.getBeanDefinition.getBeanClassName)
                    case _ => null
                  }
              }
            }
            if (null != objectClass) nameTypes.put(name, objectClass)
          } else if (classOf[Factory[_]].isAssignableFrom(beanClass)) {
            nameTypes.put(name, Factory.getObjectType(beanClass))
          } else {
            nameTypes.put(name, beanClass)
          }
        } catch {
          case e: Exception =>
        }
      }
    }
    nameTypes
  }

  /** Register a bean definition to Spring, bridging Factory to FactoryBean when needed.
   *
   * @param defn     bind definition to register
   * @param registry Spring bean definition registry
   */
  private def registerBean(defn: Binder.Definition, registry: BeanDefinitionRegistry): Unit = {
    val bd = new ExtBeanDefinition(defn, Enviroment.Default)
    if (null != defn.targetClass && !defn.isAbstract) {
      val targetClass = defn.targetClass
      if (classOf[Factory[_]].isAssignableFrom(defn.clazz) && !classOf[FactoryBean[_]].isAssignableFrom(defn.clazz)) {
        val name = defn.beanName
        registry.registerBeanDefinition(defn.beanName + "#proxy", bd)
        registry.registerBeanDefinition(name, createFactoryDefinition(defn))
      } else {
        registry.registerBeanDefinition(defn.beanName, bd)
      }
    } else {
      registry.registerBeanDefinition(defn.beanName, bd)
    }
  }

  /** Create a Spring FactoryBean definition that wraps Factory[_].
   *
   * @param defn bind definition for the factory
   * @return generic bean definition for FactoryBean proxy
   */
  private def createFactoryDefinition(defn: Definition): GenericBeanDefinition = {
    assert(defn.targetClass.nonEmpty)
    val name = defn.beanName
    val factory = new GenericBeanDefinition()
    factory.setBeanClass(classOf[FactoryBeanProxy[_]])
    factory.setAutowireMode(AbstractBeanDefinition.AUTOWIRE_NO)
    factory.setScope(defn.scope)
    factory.setPrimary(defn.primaryOf.nonEmpty)
    factory.getPropertyValues.add("target", new RuntimeBeanReference(name + "#proxy"))
    factory.getPropertyValues.add("objectType", defn.targetClass.get)
    defn.description foreach { d => factory.setDescription(d + " (Spring proxy)") }
    factory
  }

  /** Resolve bean class from registry, following parent definition chain.
   *
   * @param registry bean definition registry
   * @param name     bean name
   * @return resolved bean class or null
   */
  private def getBeanClass(registry: BeanDefinitionRegistry, name: String): Class[_] = {
    val bd = registry.getBeanDefinition(name)
    var clazz: Class[_] = getBeanClass(bd)
    if (null == clazz) {
      var currDef = bd
      while (null == clazz && null != currDef && null != currDef.getParentName) {
        val parentDef = registry.getBeanDefinition(bd.getParentName)
        clazz = getBeanClass(parentDef)
        currDef = parentDef
      }
    }
    clazz
  }

  /** Extract bean class from bean definition. */
  private def getBeanClass(bd: BeanDefinition): Class[_] = {
    var clazz: Class[_] = null
    bd match {
      case abd: AbstractBeanDefinition => if (abd.hasBeanClass) clazz = abd.getBeanClass
      case _ =>
    }
    if (null == clazz) {
      clazz = if (null != bd.getBeanClassName) ClassLoaders.load(bd.getBeanClassName) else null
    }
    clazz
  }
}
