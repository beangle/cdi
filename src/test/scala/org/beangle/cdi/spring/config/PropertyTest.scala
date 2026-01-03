package org.beangle.cdi.spring.config

import org.beangle.cdi.spring.bean.RedisService
import org.beangle.cdi.spring.context.ContextInitializer
import org.beangle.commons.logging.Logging
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class PropertyTest extends AnyFunSpec, Matchers, Logging {

  describe("PropertyResolver") {
    it("resolve env") {
      val factory = ContextInitializer.load("classpath:org/beangle/cdi/spring/context-config.xml")
      val service = factory.getBean("redisService", classOf[RedisService])
      assert(service.config.url == "host2:1234")
    }
  }
}
