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

package org.beangle.cdi.spring.beans

import java.beans.PropertyEditorSupport
import scala.annotation.nowarn
import scala.collection.mutable
import scala.jdk.javaapi.CollectionConverters

/**
 * Property editor for Scala collections, converting any source collection to a given
 * target collection type.
 */
class ScalaCollectionEditor[T, U](val builderFunc: () => mutable.Builder[T, _], val nullAsEmpty: Boolean = false)
  extends PropertyEditorSupport {

  override def setAsText(text: String): Unit = {
    setValue(text)
  }

  @nowarn
  override def setValue(value: AnyRef): Unit = {
    val builder = builderFunc()
    value match {
      case null if !nullAsEmpty =>
        super.setValue(null)
        return
      case null if nullAsEmpty => builder.clear()
      case source: IterableOnce[T] => builder ++= source
      case jcl: java.util.Collection[T] => builder ++= CollectionConverters.asScala(jcl)
      case javaMap: java.util.Map[T, U] =>
        val mapBuilder = builder.asInstanceOf[mutable.Builder[(T, U), _]]
        mapBuilder ++= CollectionConverters.asScala(javaMap)
      case el: Any => builder += el.asInstanceOf[T]
    }
    super.setValue(builder.result())
  }
}
