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

package org.beangle.cdi

import scala.collection.mutable

/**
 * Bean Container.
 *
 * @author chaostone
 * @since 3.1.0
 */
trait Container {

  def id: String

  def contains(key: Any): Boolean

  def getType(key: Any): Option[Class[_]]

  def getDefinition(key: Any): Any

  def getBean[T](key: Any): Option[T]

  def getBean[T](clazz: Class[T]): Option[T]

  def getBeans[T](clazz: Class[T]): Map[String, T]

  def keys: Set[_]

  def parent: Container

}

object Container {

  var ROOT: Container = _

  var listeners: List[ContainerListener] = Nil

  def addListener(listener: ContainerListener): Unit = {
    listeners = listener :: listeners
  }

  val containers = new mutable.HashMap[String, Container]

  def get(id: String): Container = {
    Container.containers.getOrElse(id, ROOT)
  }
}

trait ContainerAware {

  def container: Container

  def container_=(container: Container): Unit
}

trait ContainerListener {

  def onStarted(container: Container): Unit = {}

  def onStopped(container: Container): Unit = {}
}

trait PropertySource {

  def properties: collection.Map[String, String]
}
