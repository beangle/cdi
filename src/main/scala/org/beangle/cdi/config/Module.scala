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

package org.beangle.cdi.config

import org.beangle.commons.cdi.{BindModule, ReconfigModule}
import org.beangle.commons.collection.Collections
import org.beangle.commons.config.profile
import org.beangle.commons.io.IOs
import org.beangle.commons.lang.reflect.Reflections
import org.beangle.commons.lang.{ClassLoaders, Strings}

import java.io.InputStream

/** Module definition
 *
 * @param className className
 */
case class Module(className: String)

object Module {
  def load(className: String, profiles: Set[String]): Option[Any] = {
    var moduleClass = ClassLoaders.load(className)
    if (!classOf[BindModule].isAssignableFrom(moduleClass) && !classOf[ReconfigModule].isAssignableFrom(moduleClass)) {
      ClassLoaders.get(className + "$") match {
        case Some(clazz) => moduleClass = clazz
        case None => throw new RuntimeException(className + " is not a module")
      }
    }
    if (matches(moduleClass, profiles)) {
      if (moduleClass.getConstructors.length > 0) {
        Some(Reflections.newInstance(moduleClass))
      } else {
        Option(moduleClass.getDeclaredField("MODULE$").get(null))
      }
    } else {
      None
    }
  }

  private def matches(clazz: Class[_], profiles: Set[String]): Boolean = {
    val anno = clazz.getAnnotation(classOf[profile])
    null == anno || null != anno && profiles.contains(anno.value)
  }

  /** 从XML中读取配置
   *
   * @param is 输入流
   * @return
   */
  def fromXml(is: InputStream): Iterable[String] = {
    val str = IOs.readString(is)
    (scala.xml.XML.loadString(str) \ "cdi").headOption match {
      case None => List.empty
      case Some(con) =>
        val modules = Collections.newBuffer[String]
        (con \ "module") foreach { moduleElem =>
          val clazzName = (moduleElem \ "@class").text
          if (Strings.isNotBlank(clazzName)) {
            modules += clazzName
          }
        }
        modules
    }
  }
}
