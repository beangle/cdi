/*
 * Beangle, Agile Development Scaffold and Toolkits.
 *
 * Copyright © 2005, The Beangle Software.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.beangle.cdi.spring.config

import java.{util => ju}

import org.beangle.cdi.bind.Binder.{Definition, PropertyPlaceHolder, ReferenceValue}
import org.beangle.commons.collection.Collections
import org.springframework.beans.MutablePropertyValues
import org.springframework.beans.factory.config.{RuntimeBeanReference, TypedStringValue}
import org.springframework.beans.factory.support._

object ExtBeanDefinition {
  def convert(v: Any, properties: Map[String, String]): AnyRef = {
    v match {
      case value: List[_] =>
        val list = new ManagedList[Any]
        value foreach {
          case rv: ReferenceValue => list.add(new RuntimeBeanReference(rv.ref))
          case item => list.add(item)
        }
        list
      case value: Set[_] =>
        val set = new ManagedSet[Any]
        value foreach { item =>
          set.add(item match {
            case rv: ReferenceValue => new RuntimeBeanReference(rv.ref)
            case _ => item
          })
        }
        set
      case value: ju.Properties =>
        val props = new ManagedProperties()
        val propertyNames = value.propertyNames()
        while (propertyNames.hasMoreElements) {
          val key = propertyNames.nextElement().toString
          props.put(new TypedStringValue(key), new TypedStringValue(value.getProperty(key)))
        }
        props
      case value: Map[_, _] =>
        val maps = new ManagedMap[Any, Any]
        value foreach {
          case (itemk, itemv) =>
            itemv match {
              case rv: ReferenceValue => maps.put(itemk, new RuntimeBeanReference(rv.ref))
              case _ => maps.put(itemk, convert(itemv, properties))
            }
        }
        maps
      case value: Definition => new RuntimeBeanReference(value.beanName)
      case value: ReferenceValue => new RuntimeBeanReference(value.ref)
      case PropertyPlaceHolder(name, defaultValue) =>
        properties.get(name) match {
          case Some(v) => v
          case None => if (null == defaultValue) "${" + name + "}" else defaultValue
        }
      case value: AnyRef => value
    }
  }
}

class ExtBeanDefinition extends GenericBeanDefinition {

  val nowires: collection.mutable.Set[String] = Collections.newSet[String]

  val optionals: collection.mutable.Set[String] = Collections.newSet[String]

  var wiredEagerly: Boolean = _

  def this(definition: Definition, properties: Map[String, String]) = {
    this()
    this.setBeanClass(definition.clazz)
    this.setScope(definition.scope)
    if (null != definition.initMethod) this.setInitMethodName(definition.initMethod)
    val mpv = new MutablePropertyValues()
    for ((key, v) <- definition.properties) mpv.add(key, ExtBeanDefinition.convert(v, properties))

    this.setAutowireMode(AbstractBeanDefinition.AUTOWIRE_NO)
    this.setLazyInit(definition.lazyInit)
    this.setAbstract(definition.isAbstract)
    this.setParentName(definition.parent)
    this.setPrimary(definition.primary)
    this.setDescription(definition.description)
    if (null != definition.constructorArgs) {
      val cav = this.getConstructorArgumentValues
      definition.constructorArgs.foreach(arg => cav.addGenericArgumentValue(ExtBeanDefinition.convert(arg, properties)))
    }
    this.setPropertyValues(mpv)
    this.nowires ++= definition.nowires
    this.optionals ++= definition.optionals
    this.wiredEagerly = definition.wiredEagerly
  }

}

