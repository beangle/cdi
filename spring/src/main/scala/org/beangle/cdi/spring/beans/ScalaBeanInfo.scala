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

package org.beangle.cdi.spring.beans

import java.beans._
import org.beangle.commons.lang.reflect.BeanInfos

class ScalaBeanInfo(beanClass: Class[_]) extends java.beans.BeanInfo {

  def delegate: java.beans.BeanInfo = {
    Introspector.getBeanInfo(beanClass)
  }

  private val propertyDescriptors = buildProperties(beanClass)

  private def buildProperties(beanClass: Class[_]): Array[PropertyDescriptor] = {
    val descriptors = new collection.mutable.HashMap[String, PropertyDescriptor]
    val manifest = BeanInfos.get(beanClass)
    for ((name, mi) <- manifest.properties) {
      descriptors.put(name, new PropertyDescriptor(name, mi.getter.orNull, mi.setter.orNull))
    }
    descriptors.values.toArray
  }

  override def getPropertyDescriptors: Array[PropertyDescriptor] = propertyDescriptors

  override def getAdditionalBeanInfo: Array[java.beans.BeanInfo] = {
    Array.empty
  }

  override def getBeanDescriptor: BeanDescriptor = {
    delegate.getBeanDescriptor
  }

  override def getDefaultEventIndex: Int = {
    -1
  }

  override def getDefaultPropertyIndex: Int = {
    -1
  }

  override def getEventSetDescriptors: Array[EventSetDescriptor] = {
    Array.empty
  }

  override def getIcon(iconKind: Int): java.awt.Image = {
    null
  }

  override def getMethodDescriptors: Array[MethodDescriptor] = {
    delegate.getMethodDescriptors
  }

}
