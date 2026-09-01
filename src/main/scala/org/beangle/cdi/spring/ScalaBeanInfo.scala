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

import org.beangle.commons.lang.reflect.BeanInfos

import java.beans.*

/** Lightweight BeanInfo for Scala classes that bypasses JDK Introspector.
 *
 * Instead of triggering the expensive full-class JDK introspection cycle,
 * this implementation receives pre-built property descriptors from
 * [[ScalaBeanInfoFactory.buildProperties]], keeping the Spring
 * CachedIntrospectionResults cache lightweight and allocation-free.
 *
 * @param beanClass           Scala class to introspect
 * @param propertyDescriptors pre-built property descriptors (may be empty for interfaces / JDK types)
 */
class ScalaBeanInfo(beanClass: Class[_], propertyDescriptors: Array[PropertyDescriptor]) extends java.beans.BeanInfo {

  override def getPropertyDescriptors: Array[PropertyDescriptor] = propertyDescriptors

  override def getAdditionalBeanInfo: Array[java.beans.BeanInfo] = {
    Array.empty
  }

  override def getBeanDescriptor: BeanDescriptor = {
    // Spring CachedIntrospectionResults.getBeanClass() 依赖非空的 BeanDescriptor；
    // 用 new BeanDescriptor(beanClass) 替代 Introspector.getBeanInfo，避免整棵类的 JDK 内省。
    new BeanDescriptor(beanClass)
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
    // Spring 不使用 MethodDescriptor（仅 getPropertyDescriptors/getBeanDescriptor）；
    // 空数组比 null 更防御，JDK SimpleBeanInfo 则默认返回 null。
    Array.empty
  }

}
