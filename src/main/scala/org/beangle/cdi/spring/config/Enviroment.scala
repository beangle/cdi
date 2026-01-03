package org.beangle.cdi.spring.config

import org.beangle.commons.cdi.Binding.PropertyPlaceHolder
import org.beangle.commons.cdi.PropertySource
import org.beangle.commons.collection.Collections
import org.beangle.commons.lang.Strings

/** System Enviroment
 */
class Enviroment {
  private val sources = Collections.newBuffer[PropertySource]

  private val cache = Collections.newMap[String, String]

  private val processors = Collections.newBuffer[PropertySource.Processor]

  private val resolving = Collections.newBuffer[String]

  /** 单纯的属性名称
   * 不是variable:defaultValue
   *
   * @param name
   * @return
   */
  def getProperty(name: String): Option[String] = {
    cache.get(name) match {
      case None =>
        var value: String = null
        sources.find { s =>
          value = s.get(name, null)
          null != value
        }
        if (null == value) {
          None
        } else {
          val nvalue = process(name, value)
          cache.put(name, nvalue)
          Some(nvalue)
        }
      case e@Some(v) => e
    }
  }

  def interpreter(holder: PropertyPlaceHolder): String = {
    val values = holder.variables map { v =>
      if (resolving.contains(v.name)) {
        val path = resolving.addOne(v.name).mkString("->")
        resolving.clear()
        throw new RuntimeException(s"Loop and recursive parsing :$path")
      }
      resolving.addOne(v.name)
      val pv = getProperty(v.name)
      resolving.subtractOne(v.name)
      val key = "${" + v.name + "}"
      (key, pv.orElse(v.defaultValue).getOrElse("${" + v.name + "}"))
    }
    var pattern = holder.pattern
    values foreach { case (k, v) =>
      pattern = Strings.replace(pattern, k, v)
    }
    pattern
  }

  private def process(key: String, value: String): String = {
    var v = value
    if (PropertyPlaceHolder.hasVariable(value)) {
      v = interpreter(PropertyPlaceHolder(value))
    }
    processors.foreach(x => v = x.process(key, v))
    v
  }

  def addSource(source: PropertySource): Enviroment = {
    sources.addOne(source)
    this
  }

  def addSource(properties: collection.Map[String, String]): Enviroment = {
    if (properties.nonEmpty) {
      sources.addOne(new PropertySource.SimpleSource(properties))
    }
    this
  }

  def addProcessor(processor: PropertySource.Processor): Enviroment = {
    this.processors.addOne(processor)
    this
  }

  def addProcessors(processors: Iterable[PropertySource.Processor]): Enviroment = {
    this.processors.addAll(processors)
    this
  }

}
