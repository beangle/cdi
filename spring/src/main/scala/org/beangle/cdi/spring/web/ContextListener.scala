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
package org.beangle.cdi.spring.web

import org.beangle.commons.collection.Collections
import org.beangle.commons.lang.ClassLoaders.{getResources, load}
import org.beangle.commons.lang.Strings.{substringAfter, substringBefore, isNotEmpty}
import org.beangle.commons.lang.reflect.Reflections.newInstance
import org.beangle.commons.logging.Logging
import org.beangle.cdi.spring.context.{BeanFactoryLoader, ContextLoader}

import jakarta.servlet.{ServletContextEvent, ServletContextListener}
import org.beangle.cdi.Container

/**
  * 1. Disable Definition Overriding
  * 2. Default config location(spring-context.xml)
  * 3. Load children context
  */
class ContextListener extends ServletContextListener with Logging {

  var contextConfigLocation = "classpath:spring-context.xml"

  var childContextConfigLocation = ""

  var contextClassName: String = _

  private val loaders = Collections.newBuffer[ContextLoader]

  private val springContextAvaliable = !getResources("org/springframework/context/support/AbstractApplicationContext.class").isEmpty

  def loadContainer(): Container = {
    val root = newLoader().load("WebApplicationContext:ROOT", contextClassName, contextConfigLocation, null)
    //load children
    if (isNotEmpty(childContextConfigLocation)) {
      newLoader().load(substringBefore(childContextConfigLocation, "@"), contextClassName, substringAfter(childContextConfigLocation, "@"), root)
    }
    Container.containers.find { c => c.parent == Container.ROOT && c.parent != null } match {
      case Some(c) => c
      case None => throw new RuntimeException("Cannot find container from Containers")
    }
  }

  override def contextInitialized(sce: ServletContextEvent): Unit = {
    if (loaders.isEmpty) loadContainer()
  }

  override def contextDestroyed(sce: ServletContextEvent): Unit = {
    loaders.foreach { loader => loader.close() }
  }

  private def newLoader(): ContextLoader = {
    val loader =
      if (springContextAvaliable) newInstance(load("org.beangle.cdi.spring.context.ApplicationContextLoader")).asInstanceOf[ContextLoader]
      else new BeanFactoryLoader()
    loaders += loader
    loader
  }
}
