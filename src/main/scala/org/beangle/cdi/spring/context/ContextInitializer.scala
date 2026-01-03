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

package org.beangle.cdi.spring.context

import org.beangle.commons.cdi.Container
import org.beangle.commons.lang.reflect.Reflections.newInstance
import org.beangle.commons.lang.{ClassLoaders, Objects}
import org.springframework.beans.factory.BeanFactory

object ContextInitializer {
  def load(contextConfigLocation: String): BeanFactory = {
    new ContextInitializer(contextConfigLocation).load()
  }
}

/** 缺省上下文初始化器
 *
 * @param contextConfigLocation xml location(classpath:path/to/any-spring-config.xml)
 * @param contextClassName      context class name
 */
final class ContextInitializer(contextConfigLocation: String, contextClassName: String) {

  private var loader: ContextLoader = _

  def this() = {
    this("classpath:spring-context.xml", null)
  }

  def this(contextConfigLocation: String) = {
    this(Objects.nvl(contextConfigLocation, "classpath:spring-context.xml"), null)
  }

  def load(): BeanFactory = {
    newLoader().load("ROOT", contextClassName, contextConfigLocation, null)
  }

  def init(): Container = {
    load()
    Container.Default.get
  }

  def close(): Unit = {
    if null != loader then loader.close()
  }

  private def newLoader(): ContextLoader = {
    if null == loader then
      this.loader =
        if (springContextAvailable) newInstance(ClassLoaders.load("org.beangle.cdi.spring.context.ApplicationContextLoader")).asInstanceOf[ContextLoader]
        else new BeanFactoryLoader()

    this.loader
  }

  private def springContextAvailable = {
    ClassLoaders.getResources("org/springframework/context/support/AbstractApplicationContext.class").nonEmpty
  }
}
