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

import org.beangle.commons.cdi.Binder.{Definition, Reference}
import org.beangle.commons.collection.Collections
import org.beangle.commons.config.{Enviroment, PlaceHolder}
import org.springframework.beans.MutablePropertyValues
import org.springframework.beans.factory.config.{RuntimeBeanReference, TypedStringValue}
import org.springframework.beans.factory.support.*

import java.util as ju

object ExtBeanDefinition {

  /** Convert bind values to Spring-managed types for bean definition.
   *
   * @param v         value to convert (seq, set, map, properties, reference, etc.)
   * @param env       environment for placeholder resolution
   * @param mergeable whether collections should allow merge
   * @return Spring-managed value
   */
  def convert(v: Any, env: Enviroment, mergeable: Boolean = true): Any = {
    v match {
      case value: collection.Seq[_] => toList(value, mergeable)
      case value: collection.Set[_] => toSet(value, mergeable)
      case value: ju.Properties => toProperties(value, mergeable)
      case value: collection.Map[_, _] => toMap(value, env, mergeable)
      case value: Definition => new RuntimeBeanReference(value.beanName)
      case value: Reference => new RuntimeBeanReference(value.ref)
      case holder: PlaceHolder => env.interpreter(holder)
      case value: Any => value
    }
  }

  /** Convert Java Properties to Spring ManagedProperties. */
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

  /** Convert Scala Map to Spring ManagedMap, resolving references. */
  private def toMap(value: collection.Map[_, _], env: Enviroment, mergeable: Boolean): ManagedMap[Any, Any] = {
    val maps = new ManagedMap[Any, Any]
    value foreach { case (itemk, itemv) =>
      itemv match {
        case rv: Reference => maps.put(itemk, new RuntimeBeanReference(rv.ref))
        case _ => maps.put(itemk, convert(itemv, env))
      }
    }
    maps.setMergeEnabled(mergeable)
    maps
  }

  /** Convert Scala Seq to Spring ManagedList, resolving references. */
  private def toList(value: collection.Seq[_], mergeable: Boolean): ManagedList[Any] = {
    val list = new ManagedList[Any]
    value foreach {
      case rv: Reference => list.add(new RuntimeBeanReference(rv.ref))
      case item: Any => list.add(item)
    }
    list.setMergeEnabled(mergeable)
    list
  }

  /** Convert Scala Set to Spring ManagedSet, resolving references. */
  private def toSet(value: collection.Set[_], mergeable: Boolean): ManagedSet[Any] = {
    val set = new ManagedSet[Any]
    value foreach { item =>
      set.add(item match {
        case rv: Reference => new RuntimeBeanReference(rv.ref)
        case _ => item
      })
    }
    set.setMergeEnabled(mergeable)
    set
  }

}

import ExtBeanDefinition.convert

/** Extended bean definition that maps bind Definition to Spring GenericBeanDefinition.
 *
 * Supports Scala collections, optional types, and placeholder resolution.
 */
class ExtBeanDefinition extends GenericBeanDefinition {

  var beanName: String = _

  val nowires: collection.mutable.Set[String] = Collections.newSet[String]

  val optionals: collection.mutable.Set[String] = Collections.newSet[String]

  var wiredEagerly: Boolean = _

  /** Construct from bind Definition and environment.
   *
   * @param d   bind definition to convert
   * @param env environment for placeholder and reference conversion
   */
  def this(d: Definition, env: Enviroment) = {
    this()
    this.setBeanClass(d.clazz)
    this.setScope(d.scope)
    this.beanName = d.beanName
    d.initMethod foreach { m => this.setInitMethodName(m) }
    d.destroyMethod foreach { m => this.setDestroyMethodName(m) }
    d.factoryBean foreach { b => this.setFactoryBeanName(b) }
    d.factoryMethod foreach { b => this.setFactoryMethodName(b) }

    val mpv = new MutablePropertyValues()
    d.properties.foreach { (key, v) => mpv.add(key, convert(v, env)) }
    this.setPropertyValues(mpv)
    this.setAutowireMode(AbstractBeanDefinition.AUTOWIRE_NO)
    this.setLazyInit(d.lazyInit)
    this.setAbstract(d.isAbstract)
    d.parent foreach { p => this.setParentName(p) }
    this.setPrimary(d.primaryOf.nonEmpty)
    d.description foreach { d => this.setDescription(d) }
    val cav = this.getConstructorArgumentValues
    d.constructorArgs.foreach(arg => cav.addGenericArgumentValue(convert(arg, env)))
    this.nowires ++= d.nowires
    this.optionals ++= d.optionals
    this.wiredEagerly = d.wiredEagerly
  }

}
