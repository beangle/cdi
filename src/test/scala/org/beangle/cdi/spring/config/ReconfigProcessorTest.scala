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

import org.beangle.commons.cdi.Container
import org.beangle.cdi.spring.bean.*
import org.beangle.commons.lang.time.Stopwatch
import org.beangle.commons.logging.Logging
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec
import org.springframework.context.ApplicationContext
import org.springframework.context.support.ClassPathXmlApplicationContext

class ReconfigProcessorTest extends AnyFunSpec with Matchers with Logging {

  describe("ReconfigProcessor") {
    it("testGetDefinition") {
      val ctx = new ClassPathXmlApplicationContext("/org/beangle/cdi/spring/context-simple.xml")
      // two user provider
      ctx.getBean("userDaoProvider") should not be (null)

      ctx.getBean("userLdapProvider") should not be (null)

      // userService
      val userService = ctx.getBean("userService").asInstanceOf[UserService]
      userService should not be (null)
      userService.someMap should not be (null)
      userService.provider.getClass() should equal(classOf[UserDaoProvider])
      // userLdapService
      val userLdapService = ctx.getBean("userLdapService").asInstanceOf[UserService]
      userLdapService should not be (null)
      userLdapService.provider.getClass() should equal(classOf[UserLdapProvider])
    }

    val factory = new ClassPathXmlApplicationContext("/org/beangle/cdi/spring/context-config.xml")
    it("Override") {
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

    it("Get Singleton object and wire map") {
      factory.getBean("noneDao") should be(NoneDao)
      val managers = factory.getBeansOfType(classOf[ProviderManager])
      assert(!managers.isEmpty())
      assert(managers.values.iterator().next.providerMap.size == 2)
    }

    it("Test get normal and factory bean") {
      val watch = new Stopwatch(true)
      factory.getBean(classOf[Container]) should not be (null)
      factory.getBean("userLdapProvider") should not be (null)
      testBean(factory)
      testFactoryBean(factory)
      logger.debug(s"config  context-auto completed using $watch")
    }

    it("testAdvance") {
      val watch = new Stopwatch(true)
      // test Alias
      factory.getBean(classOf[TestService].getName()) should not be (null)
      val consumer = factory.getBean(classOf[ResourcesConsumer].getName()).asInstanceOf[ResourcesConsumer]
      consumer should not be (null)
      consumer.resources should not be (null)
      logger.debug(s"config  advance context-auto completed using $watch")
    }
  }

  private def testFactoryBean(factory: ApplicationContext): Unit = {
    val testService = factory.getBean("testService", classOf[TestService])
    testService should not be (null)
    testService.entityDao should not be (null)
    testService.noneDao should be(null)
    println(testService.optionDao)
    assert(testService.optionDao.isDefined)
  }

  private def testBean(factory: ApplicationContext): Unit = {
    // two user provider
    val daoProvider = factory.getBean("userDaoProvider").asInstanceOf[UserDaoProvider]
    daoProvider should not be (null)
    daoProvider.container should not be (null)

    val daoProvider2 = factory.getBean("userDaoProvider").asInstanceOf[UserDaoProvider]
    daoProvider2 should not be (null)

    daoProvider2 should equal(daoProvider)

    val ldapProvider = factory.getBean("userLdapProvider").asInstanceOf[UserLdapProvider]
    ldapProvider should not be (null)

    // userService
    val action = factory.getBean(classOf[SomeAction].getName()).asInstanceOf[SomeAction]

    action should not be (null)
    action.hasDaoProvider() should be(true)
    action.hasLdapProvider() should be(true)
    action.userDaoProvider should equal(daoProvider)
    action.ldapProvider should equal(ldapProvider)

    val action2 = factory.getBean(classOf[SomeAction].getName()).asInstanceOf[SomeAction]

    action2 should not equal (action)
  }
}
