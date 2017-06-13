/*
 * Beangle, Agile Development Scaffold and Toolkit
 *
 * Copyright (c) 2005-2017, Beangle Software.
 *
 * Beangle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Beangle is distributed in the hope that it will be useful.
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Beangle.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.beangle.cdi.spring.bean

import org.beangle.cdi.Scope
import org.beangle.cdi.bind.BindModule

object TestModule extends BindModule {

  protected def binding(): Unit = {
    bind(classOf[SomeAction]).in(Scope.Prototype)
    bind(classOf[UserLdapProvider], classOf[UserDaoProvider]).shortName()
    bind(classOf[TestService]).shortName().nowire("noneDao")

    bind(classOf[ProviderManager]).property("providers", list(ref("userDaoProvider")))

    bind(classOf[ResourcesConsumer]).property("resources",
      ";classpath*:META-INF/beangle/orm-naming.xml;classpath:beangle/orm-naming.xml")

    bind("noneDao", NoneDao)

  }

}
