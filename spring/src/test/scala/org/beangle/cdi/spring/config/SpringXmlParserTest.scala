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

import org.beangle.commons.logging.Logging
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec
import org.springframework.core.io.ClassPathResource

class SpringXmlParserTest extends AnyFunSpec with Matchers with Logging {

  describe("BeanDefinitionReader") {
    it("parse xml") {
      val holders = ReconfigReader.load(new ClassPathResource("org/beangle/cdi/spring/context-simple.xml"))
      holders.length should be (5)
    }
  }
}
