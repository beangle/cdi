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

package org.beangle.cdi.spring.web

import jakarta.servlet.{ServletContext, ServletContextEvent, ServletContextListener}
import org.beangle.cdi.Container
import org.beangle.cdi.bind.BindRegistry
import org.beangle.cdi.spring.context.{BeanFactoryLoader, ContextLoader}
import org.beangle.commons.collection.Collections
import org.beangle.commons.lang.ClassLoaders.{getResources, load}
import org.beangle.commons.lang.Strings.{isNotEmpty, substringAfter, substringBefore}
import org.beangle.commons.lang.reflect.Reflections.newInstance
import org.beangle.commons.lang.{Strings, SystemInfo}
import org.beangle.commons.logging.Logging

/**
  * 1. Disable Definition Overriding
  * 2. Default config location(spring-context.xml)
  * 3. Load children context
  */
class ContextListener extends ServletContextListener with Logging {

  var contextConfigLocation = "classpath:spring-context.xml"

  var contextClassName: String = _

  private var loader: ContextLoader = _

  private val springContextAvailable = !getResources("org/springframework/context/support/AbstractApplicationContext.class").isEmpty

  def loadContainer(sc: ServletContext): Container = {
    newLoader().load("ROOT", contextClassName, contextConfigLocation, null)
    Container.ROOT
  }

  override def contextInitialized(sce: ServletContextEvent): Unit = {
    if (null == loader) loadContainer(sce.getServletContext)
  }

  override def contextDestroyed(sce: ServletContextEvent): Unit = {
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
