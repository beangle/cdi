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
import org.beangle.commons.cdi.Container
import org.beangle.commons.event.EventMulticaster
import org.beangle.commons.lang.time.Stopwatch
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class BindModuleTest extends AnyFunSpec, Matchers {

  describe("ReconfigProcessor") {
    val factory = SpringContainerLoader.load()
    it("Override") {
      // userService
      val userService = factory.getBean[UserService]("userService").get

      // unmerged map
      userService.someMap should not be null
      userService.someMap.size should be(1)
      userService.someMap("string") should be("override string")
      // merged list
      userService.someList.size should be(1)

      // change class
      val ldapProvider = factory.getBean[UserLdapProvider]("userLdapProvider").orNull
      ldapProvider.isInstanceOf[AdvancedUserLdapProvider] should be(true)

      val userLdapService = factory.getBean[UserService]("userLdapService").get

      userLdapService.provider.getClass should equal(classOf[AdvancedUserLdapProvider])
    }

    it("Get Singleton object and wire map") {
      assert(factory.getBean("noneDao").contains(NoneDao))
      val managers = factory.getBeans(classOf[ProviderManager])
      assert(managers.nonEmpty)
      assert(managers.head._2.providerMap.size == 2)
    }

    it("Test get normal and factory bean") {
      val watch = new Stopwatch(true)
      factory.getBeans(classOf[Container]).size should equal(1)
      factory.getBean("userLdapProvider").size should equal(1)
      testBean(factory)
      testFactoryBean(factory)
      testMulticaster(factory)
    }
    it("test getTypes"){
      val types = factory.beanTypes
      println(types.mkString("\n"))
    }
  }

  private def testFactoryBean(factory: Container): Unit = {
    val ts = factory.getBean[TestService]("testService")
    ts.isDefined should be(true)
    val testService = ts.get
    testService.entityDao should not be null
    testService.noneDao should be(null)
    assert(testService.optionDao.isDefined)
  }

  private def testBean(factory: Container): Unit = {
    // two user provider
    val daoProvider = factory.getBean[UserDaoProvider]("userDaoProvider").orNull
    daoProvider should not be null
    daoProvider.container should not be null

    val daoProvider2 = factory.getBean[UserDaoProvider]("userDaoProvider").orNull
    daoProvider2 should not be null

    daoProvider2 should equal(daoProvider)

    val ldapProvider = factory.getBean[UserLdapProvider]("userLdapProvider").orNull
    ldapProvider should not be null

    // userService
    val action = factory.getBean[SomeAction](classOf[SomeAction].getName).orNull

    action should not be null
    action.hasDaoProvider() should be(true)
    action.hasLdapProvider() should be(true)
    action.userDaoProvider should equal(daoProvider)
    action.ldapProvider should equal(ldapProvider)

    val action2: SomeAction = factory.getBean(classOf[SomeAction].getName).orNull

    action2 should not be null
    action2 should not equal action
  }

  private def testMulticaster(factory: Container): Unit = {
    val beans = factory.getBeans(classOf[EventMulticaster])
    beans.size should equal(1)
  }

}
