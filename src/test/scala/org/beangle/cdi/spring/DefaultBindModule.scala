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

package org.beangle.cdi.spring

import org.beangle.cdi.spring.bean.*
import org.beangle.commons.cdi.{BindModule, Scope}

object DefaultBindModule extends BindModule {

  protected def binding(): Unit = {
    System.setProperty("redis.host", "host2")
    System.setProperty("redis.port", "1234")

    bind("userDaoProvider", classOf[UserDaoProvider])
    bind("userLdapService", classOf[UserService]).property("provider", ref("userLdapProvider"))
    bind("userLdapProvider", classOf[UserLdapProvider])
    bind("userService", classOf[UserService])
      .property("someMap", map("string" -> "just some string", "ref" -> ref("userLdapProvider")))
      .property("someList", list("just another string", ref("userLdapProvider")))
      .property("provider", ref("userDaoProvider"))

    bind("entityDao", classOf[TestDaoFactory]).proxy("target", classOf[TestEntityDao])

    bind(classOf[SomeAction]).in(Scope.Prototype)
    bind(classOf[UserLdapProvider], classOf[UserDaoProvider]).shortName().onMissing()
    bind(classOf[TestService]).shortName().nowire("noneDao")

    bind(classOf[ProviderManager]).property("providers", list(ref("userDaoProvider")))

    bind("noneDao", NoneDao)

    bind(classOf[OptionDao])

    bind(classOf[RedisConfig])
      .property("host", $("redis.host:localhost"))
      .property("port", $("redis.port:6378"))
      .property("url", $("${redis.host}:${redis.port}"))

    bind("redisService", classOf[RedisService])
  }
}
