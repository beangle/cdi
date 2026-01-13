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

import org.beangle.commons.cdi.Binding.PropertyPlaceHolder
import org.beangle.commons.cdi.PropertySource
import org.beangle.commons.collection.Collections
import org.beangle.commons.lang.Strings

/** System Enviroment
 */
class Enviroment {
  private val sources = Collections.newBuffer[PropertySource]

  private val cache = Collections.newMap[String, String]

  private val processors = Collections.newBuffer[PropertySource.Processor]

  private val resolving = Collections.newBuffer[String]

  /** 单纯的属性名称
   * 不是variable:defaultValue
   *
   * @param name
   * @return
   */
  def getProperty(name: String): Option[String] = {
    cache.get(name) match {
      case None =>
        var value: String = null
        sources.find { s =>
          value = s.get(name, null)
          null != value
        }
        if (null == value) {
          None
        } else {
          val nvalue = process(name, value)
          cache.put(name, nvalue)
          Some(nvalue)
        }
      case e@Some(v) => e
    }
  }

  def interpreter(holder: PropertyPlaceHolder): String = {
    val values = holder.variables map { v =>
      if (resolving.contains(v.name)) {
        val path = resolving.addOne(v.name).mkString("->")
        resolving.clear()
        throw new RuntimeException(s"Loop and recursive parsing :$path")
      }
      resolving.addOne(v.name)
      val pv = getProperty(v.name)
      resolving.subtractOne(v.name)
      val key = "${" + v.name + "}"
      (key, pv.orElse(v.defaultValue).getOrElse("${" + v.name + "}"))
    }
    var pattern = holder.pattern
    values foreach { case (k, v) =>
      pattern = Strings.replace(pattern, k, v)
    }
    pattern
  }

  private def process(key: String, value: String): String = {
    var v = value
    if (PropertyPlaceHolder.hasVariable(value)) {
      v = interpreter(PropertyPlaceHolder(value))
    }
    processors.foreach(x => v = x.process(key, v))
    v
  }

  def addSource(source: PropertySource): Enviroment = {
    sources.addOne(source)
    this
  }

  def addSource(properties: collection.Map[String, String]): Enviroment = {
    if (properties.nonEmpty) {
      sources.addOne(new PropertySource.SimpleSource(properties))
    }
    this
  }

  def addProcessor(processor: PropertySource.Processor): Enviroment = {
    this.processors.addOne(processor)
    this
  }

  def addProcessors(processors: Iterable[PropertySource.Processor]): Enviroment = {
    this.processors.addAll(processors)
    this
  }
}

/** Extract property from Env
 */
object EnvPropertySource extends PropertySource {
  private def toEnvName(name: String): String = {
    name.replace('.', '_').toUpperCase
  }

  override def contains(name: String): Boolean = {
    var contained = System.getenv(name) != null
    if (!contained) {
      contained = System.getenv(toEnvName(name)) != null
    }
    contained
  }

  override def get(name: String): Option[String] = {
    var v = System.getenv(name)
    if (null == v) {
      v = System.getenv(toEnvName(name))
    }
    Option(v)
  }

  override def get(name: String, defaultValue: String): String = {
    get(name).getOrElse(defaultValue)
  }
}
