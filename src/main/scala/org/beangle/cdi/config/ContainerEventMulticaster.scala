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

import org.beangle.commons.bean.Initializing
import org.beangle.commons.cdi.{CdiEventListener, Container}
import org.beangle.commons.event.DefaultEventMulticaster

/** 将容器中的监听器搜集起来，集中注册到时间广播中
 *
 * @author chaostone
 */
class ContainerEventMulticaster extends DefaultEventMulticaster, Initializing {

  var container: Container = _

  override def init(): Unit = {
    container.getBeans(classOf[CdiEventListener[_]]) foreach { e => addListener(e._2) }
  }
}
