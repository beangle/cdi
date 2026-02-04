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

import org.beangle.cdi.config.ReconfigParser
import org.beangle.commons.lang.ClassLoaders
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ReconfigParserTest extends AnyFunSpec, Matchers {

  describe("ReconfigParser") {
    it("parse xml") {
      val holders2 = ReconfigParser.load(ClassLoaders.getResource("org/beangle/cdi/spring/reconfig.xml").get)
      holders2.length should be(3)
      val properties = holders2.find(_.beanName == "properties")
      properties.nonEmpty should be(true)

      properties.get.properties.contains("a.b.c") should be(true)
      properties.get.properties.get("a.b.c").contains("1") should be(true)
    }
  }
}
