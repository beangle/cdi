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

import org.beangle.commons.cdi.{BindModule, ReconfigModule, profile}
import org.beangle.commons.collection.Collections
import org.beangle.commons.io.IOs
import org.beangle.commons.lang.reflect.Reflections
import org.beangle.commons.lang.{ClassLoaders, Strings}

import java.io.InputStream

/** Module definition
 *
 * @param className className
 */
case class Module(className: String) {

  def loadModule(): Any = {
    var moduleClass = ClassLoaders.load(className)
    if (!classOf[BindModule].isAssignableFrom(moduleClass) && !classOf[ReconfigModule].isAssignableFrom(moduleClass)) {
      ClassLoaders.get(className + "$") match {
        case Some(clazz) => moduleClass = clazz
        case None => throw new RuntimeException(className + " is not a module")
      }
    }
    if (moduleClass.getConstructors.length > 0) {
      Reflections.newInstance(moduleClass)
    } else {
      moduleClass.getDeclaredField("MODULE$").get(null)
    }
  }

  def matches(module: Any, profiles: Set[String]): Boolean = {
    val anno = module.getClass.getAnnotation(classOf[profile])
    null == anno || null != anno && new ProfileMatcher(anno.value).matches(profiles)
  }

}

case class CDI(name: String, modules: Seq[Module]) {

}

object CDI {
  /** 从XML中读取配置
   *
   * @param is 输入流
   * @return
   */
  def fromXml(is: InputStream): Seq[CDI] = {
    val cdis = (scala.xml.XML.load(is) \ "cdi") map { con =>
      var containerName = (con \ "@name").text
      if (Strings.isEmpty(containerName)) containerName = "default"
      val modules = Collections.newBuffer[Module]
      (con \ "module") foreach { moduleElem =>
        val clazzName = (moduleElem \ "@class").text
        if (Strings.isNotBlank(clazzName)) {
          modules += Module(clazzName)
        }
      }
      CDI(containerName, modules.toSeq)
    }
    IOs.close(is)
    cdis
  }
}
