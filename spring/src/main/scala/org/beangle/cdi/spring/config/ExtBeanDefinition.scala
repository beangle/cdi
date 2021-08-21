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

import org.beangle.cdi.bind.Binding.{Definition, PropertyPlaceHolder, ReferenceValue}
import org.beangle.commons.collection.Collections
import org.springframework.beans.MutablePropertyValues
import org.springframework.beans.factory.config.{RuntimeBeanReference, TypedStringValue}
import org.springframework.beans.factory.support._

import java.{util => ju}

object ExtBeanDefinition {
  def convert(v: Any, properties: collection.Map[String, String], mergeable: Boolean = true): Any = {
    v match {
      case value: collection.Seq[_] => toList(value, mergeable)
      case value: collection.Set[_] => toSet(value, mergeable)
      case value: ju.Properties => toProperties(value, mergeable)
      case value: collection.Map[_, _] => toMap(value, properties, mergeable)
      case value: Definition => new RuntimeBeanReference(value.beanName)
      case value: ReferenceValue => new RuntimeBeanReference(value.ref)
      case PropertyPlaceHolder(name, defaultValue) =>
        properties.get(name) match {
          case Some(v) => v
          case None => if (null == defaultValue) "${" + name + "}" else defaultValue
        }
      case value: Any => value
    }
  }

  private def toProperties(value: ju.Properties, mergeable: Boolean): ManagedProperties = {
    val props = new ManagedProperties()
    val propertyNames = value.propertyNames()
    while (propertyNames.hasMoreElements) {
      val key = propertyNames.nextElement().toString
      props.put(new TypedStringValue(key), new TypedStringValue(value.getProperty(key)))
    }
    props.setMergeEnabled(mergeable)
    props
  }

  private def toMap(value: collection.Map[_, _], properties: collection.Map[String, String], mergeable: Boolean): ManagedMap[Any, Any] = {
    val maps = new ManagedMap[Any, Any]
    value foreach { case (itemk, itemv) =>
      itemv match {
        case rv: ReferenceValue => maps.put(itemk, new RuntimeBeanReference(rv.ref))
        case _ => maps.put(itemk, convert(itemv, properties))
      }
    }
    maps.setMergeEnabled(mergeable)
    maps
  }

  private def toList(value: collection.Seq[_], mergeable: Boolean): ManagedList[Any] = {
    val list = new ManagedList[Any]
    value foreach {
      case rv: ReferenceValue => list.add(new RuntimeBeanReference(rv.ref))
      case item: Any => list.add(item)
    }
    list.setMergeEnabled(mergeable)
    list
  }

  private def toSet(value: collection.Set[_], mergeable: Boolean): ManagedSet[Any] = {
    val set = new ManagedSet[Any]
    value foreach { item =>
      set.add(item match {
        case rv: ReferenceValue => new RuntimeBeanReference(rv.ref)
        case _ => item
      })
    }
    set.setMergeEnabled(mergeable)
    set
  }
}

import ExtBeanDefinition.convert
class ExtBeanDefinition extends GenericBeanDefinition {

  val nowires: collection.mutable.Set[String] = Collections.newSet[String]

  val optionals: collection.mutable.Set[String] = Collections.newSet[String]

  var wiredEagerly: Boolean = _

  def this(d: Definition, properties: collection.Map[String, String]) = {
    this()
    this.setBeanClass(d.clazz)
    this.setScope(d.scope)
    if (null != d.initMethod) this.setInitMethodName(d.initMethod)
    if (null != d.destroyMethod) this.setDestroyMethodName(d.destroyMethod)
    if (null != d.factoryBean) this.setFactoryBeanName(d.factoryBean)
    if (null != d.factoryMethod) this.setFactoryMethodName(d.factoryMethod)
    val mpv = new MutablePropertyValues()
    for ((key, v) <- d.properties) {
      mpv.add(key, convert(v, properties))
    }
    this.setPropertyValues(mpv)
    this.setAutowireMode(AbstractBeanDefinition.AUTOWIRE_NO)
    this.setLazyInit(d.lazyInit)
    this.setAbstract(d.isAbstract)
    this.setParentName(d.parent)
    this.setPrimary(d.primary)
    this.setDescription(d.description)
    if (null != d.constructorArgs) {
      val cav = this.getConstructorArgumentValues
      d.constructorArgs.foreach(arg => cav.addGenericArgumentValue(convert(arg, properties)))
    }
    this.nowires ++= d.nowires
    this.optionals ++= d.optionals
    this.wiredEagerly = d.wiredEagerly
  }

}
