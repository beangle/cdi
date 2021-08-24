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

import java.beans.PropertyEditor

import scala.util.matching.Regex
import scala.collection.mutable
import scala.collection.immutable
import org.springframework.beans.{PropertyEditorRegistrar, PropertyEditorRegistry}

/** Property editor registrar for Scala property editors.
  *
  */
class ScalaEditorRegistrar extends PropertyEditorRegistrar {

  def registerCustomEditors(registry: PropertyEditorRegistry): Unit = {
    implicit val r = registry
    register(classOf[Option[Any]], new OptionEditor())
    // Options
    register(classOf[Option[Any]], new OptionEditor())
    // Types
    register(classOf[Regex], new RegexEditor())

    //Iterable
    register(classOf[collection.Iterable[Any]], new ScalaCollectionEditor(() => collection.Seq.newBuilder[Any]))

    // Seq
    register(classOf[collection.Seq[Any]], new ScalaCollectionEditor(() => collection.Seq.newBuilder[Any]))
    register(classOf[immutable.Seq[Any]], new ScalaCollectionEditor(() => collection.immutable.Seq.newBuilder[Any]))
    register(classOf[mutable.Seq[Any]], new ScalaCollectionEditor(() => mutable.Seq.newBuilder[Any]))

    // IndexedSeq
    register(classOf[collection.IndexedSeq[Any]], new ScalaCollectionEditor(() => collection.IndexedSeq.newBuilder[Any]))
    register(classOf[immutable.IndexedSeq[Any]], new ScalaCollectionEditor(() => immutable.IndexedSeq.newBuilder[Any]))
    register(classOf[mutable.IndexedSeq[Any]], new ScalaCollectionEditor(() => mutable.IndexedSeq.newBuilder[Any]))
    // LinearSeq
    register(classOf[collection.LinearSeq[Any]], new ScalaCollectionEditor(() => collection.LinearSeq.newBuilder[Any]))
    register(classOf[immutable.LinearSeq[Any]], new ScalaCollectionEditor(() => immutable.LinearSeq.newBuilder[Any]))

    // Buffer
    register(classOf[mutable.Buffer[Any]], new ScalaCollectionEditor(() => mutable.Buffer.newBuilder[Any]))
    register(classOf[mutable.ListBuffer[Any]], new ScalaCollectionEditor(() => mutable.ListBuffer.newBuilder[Any]))

    // Set
    register(classOf[collection.Set[Any]], new ScalaCollectionEditor(() => collection.Set.newBuilder[Any]))
    register(classOf[immutable.Set[Any]], new ScalaCollectionEditor(() => immutable.Set.newBuilder[Any]))
    register(classOf[mutable.Set[Any]], new ScalaCollectionEditor(() => mutable.Set.newBuilder[Any]))

    // Map
    register(classOf[collection.Map[Any, Any]], new ScalaCollectionEditor(() => collection.Map.newBuilder[Any, Any]))
    register(classOf[immutable.Map[Any, Any]], new ScalaCollectionEditor(() => immutable.Map.newBuilder[Any, Any]))
    register(classOf[mutable.Map[Any, Any]], new ScalaCollectionEditor(() => mutable.Map.newBuilder[Any, Any]))
    register(classOf[mutable.HashMap[Any, Any]], new ScalaCollectionEditor(() => mutable.HashMap.newBuilder[Any, Any]))
  }

  @inline
  private def register(clazz: Class[_], editor: PropertyEditor)(implicit registry: PropertyEditorRegistry): Unit = {
    registry.registerCustomEditor(clazz, editor)
  }
}
