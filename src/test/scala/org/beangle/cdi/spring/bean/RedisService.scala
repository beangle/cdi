package org.beangle.cdi.spring.bean

import org.beangle.commons.bean.Initializing

class RedisService extends Initializing {

  var config: RedisConfig = _

  override def init(): Unit = {
    println(config.host)
  }

}
