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
import org.beangle.commons.lang.ClassLoaders.{getResources, load}
import org.beangle.commons.lang.Objects
import org.beangle.commons.lang.reflect.Reflections.newInstance

object DefaultContextInitializer {
  def apply(contextConfigLocation: String, contextClassName: String): DefaultContextInitializer = {
    new DefaultContextInitializer(Objects.nvl(contextConfigLocation, "classpath:spring-context.xml"), contextClassName)
  }
}

/** 缺省上下文初始化器
 *
 * @param contextConfigLocation
 * @param contextClassName
 */
class DefaultContextInitializer(val contextConfigLocation: String, val contextClassName: String) {

  private var loader: ContextLoader = _

  private val springContextAvailable = getResources("org/springframework/context/support/AbstractApplicationContext.class").nonEmpty

  def init(): Container = {
    newLoader().load("ROOT", contextClassName, contextConfigLocation, null)
    Container.Default.get
  }

  def close(): Unit = {
    if null != loader then loader.close()
  }

  private def newLoader(): ContextLoader = {
    if null == loader then
      this.loader =
        if (springContextAvailable) newInstance(load("org.beangle.cdi.spring.context.ApplicationContextLoader")).asInstanceOf[ContextLoader]
        else new BeanFactoryLoader()

    this.loader
  }
}
