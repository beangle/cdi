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

import org.beangle.commons.cdi.{BindModule, Binder, Reconfig, ReconfigModule}
import org.beangle.commons.collection.Collections
import org.beangle.commons.config.{Enviroment, XmlConfigs, profile}
import org.beangle.commons.lang.time.Stopwatch
import org.beangle.commons.lang.{ClassLoaders, JVM, Strings}
import org.beangle.commons.net.Networks
import org.beangle.commons.net.http.HttpUtils

import java.io.File
import java.net.URL
import scala.collection.mutable

object BindingLoader {

  /** Load definition from modules and reconfig modules
   *
   * @param path classpath*:beangle.xml/classpath:beangle.xml/file://
   */
  def loadModules(path: String): (Iterable[BindModule], Iterable[Reconfig]) = {
    var profile = System.getProperty(Enviroment.ProfileKey, "")
    if (JVM.isDebugMode) profile += ",dev"
    val profiles = Strings.split(profile, ",").map(s => s.trim).toSet

    //collect bind modules and read reconfig properties
    val modules = new collection.mutable.HashSet[BindModule]
    val reconfigs = Collections.newBuffer[Reconfig]

    val moduleNames = Collections.newBuffer[String]
    val doc = XmlConfigs.load(path)
    (doc \ "cdi" \ "module") foreach { m =>
      val clazzName = (m \ "@class").text
      if Strings.isNotBlank(clazzName) then moduleNames += clazzName
    }

    moduleNames foreach { name =>
      Module.load(name, profiles) foreach {
        case bm: BindModule => modules += bm
        case rm: ReconfigModule =>
          val recfg = new Reconfig
          rm.configure(recfg)
          readReconfig(rm.configUrl, recfg)
          reconfigs += recfg
      }
    }
    (modules, reconfigs)
  }

  /** 从模块中加载bean定义 */
  def loadRegistryItems(modules: Iterable[BindModule]): mutable.Buffer[Binder.RegistryItem] = {
    val items = Collections.newBuffer[Binder.RegistryItem]
    //搜集所有模块的注册条目
    modules foreach { module =>
      val moduleName = module.getClass.getName
      val binder = new Binder(moduleName)
      module.configure(binder)
      val profile = module.getClass.getAnnotation(classOf[profile])
      val profileName = if profile == null then null else profile.value()
      items.addAll(binder.singletons.map(e => e.activeOn(profileName).locateAt(moduleName)))
      items.addAll(binder.definitions.map(e => e.activeOn(profileName).locateAt(moduleName)))
    }
    items
  }

  /** Read spring style config.xml
   *
   * @param configUrl http or file or classpath
   * @param reconfig  reconfig module
   */
  private def readReconfig(configUrl: String, reconfig: Reconfig): Unit = {
    var url: URL = null
    if (Strings.isNotBlank(configUrl)) {
      if (configUrl.startsWith("http")) {
        url = if HttpUtils.isAlive(configUrl) then Networks.url(configUrl) else null //ignore http 404
      } else if (configUrl.startsWith("file://")) {
        val file = new File(configUrl.substring("file://".length))
        url = if file.exists() then file.toURI.toURL else null //ignore not exists file
      } else if (configUrl.startsWith("classpath:")) {
        ClassLoaders.getResource(configUrl.substring("classpath:".length)) match
          case None => url = null
          case Some(u) => url = u.toURI.toURL
      } else {
        throw new RuntimeException("cannot recognize url:" + configUrl)
      }
    }
    if (null != url) {
      val watch = new Stopwatch(true)
      val holders = ReconfigParser.load(url)
      for (holder <- holders) {
        reconfig.definitions.put(holder.beanName, holder)
      }
    }
  }
}
