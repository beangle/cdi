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

import org.beangle.cdi.spring.bean.RedisService
import org.beangle.cdi.spring.context.ContextLoader
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class PropertyTest extends AnyFunSpec, Matchers {

  describe("PropertyResolver") {
    it("resolve env") {
      val factory = ContextLoader.defaultLoader.load("root", null, "classpath:org/beangle/cdi/spring/context-config.xml")
      val service = factory.getBean("redisService", classOf[RedisService])
      assert(service.config.url == "host2:1234")
    }
  }
}
