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

import org.beangle.commons.cdi.Container
import org.beangle.commons.collection.Collections
import org.beangle.commons.lang.annotation.description
import org.springframework.beans.factory.NoSuchBeanDefinitionException
import org.springframework.beans.factory.support.DefaultListableBeanFactory

import scala.jdk.javaapi.CollectionConverters.asScala

/** Spring-based IoC container implementation.
 *
 * @param factory         Spring bean factory
 * @param configLocation  path to bind module configuration (default: classpath*:beangle.xml)
 *
 * @author chaostone
 * @since 3.1.0
 */
@description("Spring-based Bean container")
class SpringContainer(private val factory: DefaultListableBeanFactory, configLocation: String = "classpath*:beangle.xml")
  extends BindModuleProcessor(configLocation), Container {

  override def id: String = factory.getSerializationId

  override def getType(key: String): Option[Class[_]] = {
    try {
      Option(factory.getType(key))
    } catch {
      case e: NoSuchBeanDefinitionException => None
    }
  }

  override def contains(key: String): Boolean = {
    factory.containsBean(key)
  }

  override def getBean[T](key: String): Option[T] = {
    try Some(factory.getBean(key).asInstanceOf[T])
    catch case _: NoSuchBeanDefinitionException => None
  }

  override def getBean[T](clazz: Class[T]): Option[T] = {
    try Some(factory.getBean(clazz))
    catch case _: NoSuchBeanDefinitionException => None
  }

  override def getBeans[T](clazz: Class[T]): Map[String, T] = {
    asScala(factory.getBeansOfType(clazz)).toMap
  }

  override def beanTypes: collection.Map[String, Class[_]] = {
    val types = Collections.newMap[String, Class[_]]
    val nameIter = factory.getBeanNamesIterator
    while (nameIter.hasNext) {
      val name = nameIter.next()
      if (!name.contains("#")) {
        val clazz = factory.getType(name)
        if (null != clazz) types.put(name, clazz)
      }
    }
    types
  }

  override def close(): Unit = {
    factory.destroySingletons()
  }

  override def underlying: AnyRef = {
    factory
  }
}
