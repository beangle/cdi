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

import org.beangle.commons.event.{DefaultEventMulticaster, Event, EventMulticaster, EventListener}
import org.beangle.cdi.{Container, ContainerListener}
import org.beangle.commons.bean.Initializing

/**
  * @author chaostone
  *
  */
class HierarchicalEventMulticaster extends DefaultEventMulticaster with Initializing {

  var parent: EventMulticaster = _
  var container: Container = _

  override def init(): Unit = {

    container.getBeans(classOf[EventListener[_]]) foreach { e =>
      addListener(e._2)
    }
  }

  override def multicast(e: Event): Unit = {
    super.multicast(e)
    if (null != parent) parent.multicast(e)
  }

}
