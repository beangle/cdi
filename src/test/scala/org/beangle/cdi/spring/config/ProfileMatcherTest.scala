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

import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec

class ProfileMatcherTest extends AnyFunSpec with Matchers {

  describe("Profile Matcher") {
    it("match success") {
      val matcher = new ProfileMatcher(" school1(dev ,test (d,d2)), school2 ")
      assert(matcher.matches("school2"))
      assert(!matcher.matches("school1 "))
      assert(matcher.matches("school1 , dev"))
      assert(!matcher.matches("school1, test"))
      assert(matcher.matches("school1, test, d2"))

      val matcher2 = new ProfileMatcher("production,dev")
      assert(matcher2.matches("dev"))
      assert(!matcher2.matches("productor"))
    }
  }
}
