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

import org.beangle.cdi.config.BindingRegistry
import org.beangle.cdi.spring.bean.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.springframework.beans.factory.support.DefaultListableBeanFactory
import org.springframework.beans.factory.xml.XmlBeanDefinitionReader
import org.springframework.context.support.ClassPathXmlApplicationContext
import org.springframework.core.io.ClassPathResource

class BeanRegistryTest extends AnyFunSpec, Matchers {

  describe("BeanFactory") {
    it("parse xml and get bean") {
      val ctx = new ClassPathXmlApplicationContext("/org/beangle/cdi/spring/context-simple.xml")
      // two user provider
      ctx.getBean("userDaoProvider") should not be (null)
      ctx.getBean("userLdapProvider") should not be (null)

      // userService
      val userService = ctx.getBean("userService").asInstanceOf[UserService]
      userService should not be (null)
      userService.someMap should not be (null)
      userService.provider.getClass should equal(classOf[UserDaoProvider])
      // userLdapService
      val userLdapService = ctx.getBean("userLdapService").asInstanceOf[UserService]
      userLdapService should not be (null)
      userLdapService.provider.getClass should equal(classOf[UserLdapProvider])
    }
    it("find definitions") {
      val factory = new DefaultListableBeanFactory()
      val reader = new XmlBeanDefinitionReader(factory)
      reader.loadBeanDefinitions(new ClassPathResource("/org/beangle/cdi/spring/context-simple.xml"))

      val existed = SpringBeanRegistry.findBeans(factory)
      val registry = new BindingRegistry(existed)
      val names = registry.getBeanNames(classOf[TestDao])

      names should not be null
      names.size should be(1)

      names.contains("entityDao") should be(true)

      val name2s = registry.getBeanNames(classOf[TestDaoFactory])
      name2s should not be null
      name2s.size should be(0)
      name2s.contains("&entityDao") should be(false)
    }
  }
}
