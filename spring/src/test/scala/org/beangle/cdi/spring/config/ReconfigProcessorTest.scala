/*
 * Beangle, Agile Development Scaffold and Toolkits.
 *
 * Copyright © 2005, The Beangle Software.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.beangle.cdi.spring.config

import org.beangle.cdi.spring.bean._
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.junit.JUnitRunner
import org.springframework.context.support.ClassPathXmlApplicationContext

@RunWith(classOf[JUnitRunner])
class ReconfigProcessorTest extends AnyFunSpec with Matchers {

  describe("ReconfigProcessor") {
    it("testGetDefinition") {
      val factory = new ClassPathXmlApplicationContext("/org/beangle/cdi/spring/context.xml")
      // two user provider
      factory.getBean("userDaoProvider") should not be (null)

      factory.getBean("userLdapProvider") should not be (null)

      // userService
      val userService = factory.getBean("userService").asInstanceOf[UserService]

      userService should not be (null)

      userService.someMap should not be (null)

      userService.provider.getClass() should equal(classOf[UserDaoProvider])

      // userLdapService
      val userLdapService = factory.getBean("userLdapService").asInstanceOf[UserService]

      userLdapService should not be (null)

      userLdapService.provider.getClass() should equal(classOf[UserLdapProvider])
    }

    it("Override") {
      val factory = new ClassPathXmlApplicationContext("/org/beangle/cdi/spring/context-config.xml")
      // userService
      val userService = factory.getBean("userService").asInstanceOf[UserService]

      userService should not be (null)

      // unmerged map
      userService.someMap should not be (null)

      userService.someMap.size should be(1)

      userService.someMap("string") should be("override string")

      // merged list
      userService.someList.size should be(3)

      // change class
      val ldapProvider = factory.getBean("userLdapProvider").asInstanceOf[UserLdapProvider]
      ldapProvider.isInstanceOf[AdvancedUserLdapProvider] should be(true)

      val userLdapService = factory.getBean("userLdapService").asInstanceOf[UserService]
      userLdapService should not be (null)

      userLdapService.provider.getClass() should equal(classOf[AdvancedUserLdapProvider])
    }

    it("Get Singleton object") {
      val factory = new ClassPathXmlApplicationContext("/org/beangle/cdi/spring/context-config.xml")
      factory.getBean("noneDao") should be(NoneDao)
    }

    it("Auto wire map") {
      val factory = new ClassPathXmlApplicationContext("/org/beangle/cdi/spring/context-config.xml")
      val managers = factory.getBeansOfType(classOf[ProviderManager])
      assert(!managers.isEmpty())
      assert(managers.values.iterator().next.providerMap.size == 2)
    }
  }
}
