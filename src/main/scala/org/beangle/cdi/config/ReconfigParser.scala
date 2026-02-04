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

package org.beangle.cdi.config

import org.beangle.cdi.CDILogger
import org.beangle.cdi.config.ReconfigParser.*
import org.beangle.commons.cdi.Binder.{InjectPlaceHolder, Reference}
import org.beangle.commons.cdi.Reconfig
import org.beangle.commons.cdi.Reconfig.{Definition, ReconfigType}
import org.beangle.commons.collection.Collections
import org.beangle.commons.conversion.impl.DefaultConversion
import org.beangle.commons.io.IOs
import org.beangle.commons.lang.{ClassLoaders, Strings}
import org.beangle.commons.xml.{Document, Element, Node}

import java.net.URL
import java.util.Properties
import scala.collection.mutable

/** Reconfig BeanDefinition Parser
 *
 * @author chaostone
 */
class ReconfigParser {

  /** Stores all used bean names so we can enforce uniqueness on a per file basis. */
  private val usedNames = new mutable.HashSet[String]

  /** Parses the supplied <code>&ltbean&gt</code> element. May return <code>null</code> if there
   * were errors during parse.
   */
  def parseBeanDefinition(ele: Node): Reconfig.Definition = {
    parseBeanDefinition(ele, null)
  }

  /** Parses the supplied <code>&ltbean&gt</code> element. May return <code>null</code> if there
   * were errors during parse.
   */
  private def parseBeanDefinition(ele: Node, containingBean: Definition): Reconfig.Definition = {
    val id = ele.getAttribute("id").orNull
    if (null == id) error("missing id", ele)
    if (containingBean == null) checkNameUniqueness(id, ele)
    parseBeanDefinition(ele, id, containingBean)
  }

  /** Validate that the specified bean name and aliases have not been used already. */
  private def checkNameUniqueness(beanName: String, beanElement: Node): Unit = {
    if (this.usedNames.contains(beanName)) error("Bean name '" + beanName + "' is already used in this file", beanElement)
    this.usedNames += beanName
  }

  /** Parse the bean definition itself, without regard to name or aliases. May
   * return <code>null</code> if problems occured during the parse of the bean
   * definition.
   */
  private def parseBeanDefinition(ele: Node, beanName: String, containingBean: Definition): Definition = {
    val className = ele.getAttribute("class").orNull
    try {
      val configType = if ele.getAttribute("override").contains("remove") then ReconfigType.Remove else ReconfigType.Update
      val bd = new Definition(beanName, configType)
      bd.clazz = Option(if (null == className) null else ClassLoaders.load(className))
      ele.getAttribute("primary-of") foreach { p =>
        bd.primaryOf(Strings.split(p).map(ClassLoaders.load(_)): _*)
      }
      parseConstructorArgs(ele, bd)
      parseProperties(ele, bd)
      return bd
    } catch {
      case ex: ClassNotFoundException => error("Bean class [" + className + "] not found", ele, ex)
      case exr: Throwable => error("Unexpected failure during bean definition parsing", ele, exr)
    }
    null
  }

  /** Parse constructor-arg sub-elements of the given bean element. */
  private def parseConstructorArgs(beanEle: Node, bd: Definition): Unit = {
    (beanEle \ "constructor-arg") foreach { node => parseConstructorArg(node, bd) }
  }

  /** Parse property sub-elements of the given bean element. */
  private def parseProperties(beanEle: Node, bd: Definition): Unit = {
    (beanEle \ "property") foreach { node => parseProperty(node, bd) }
  }

  /** Parse a constructor-arg element. */
  private def parseConstructorArg(ele: Node, bd: Definition): Unit = {
    val indexAttr = ele.getAttribute("index", null)
    if (Strings.isNotEmpty(indexAttr)) {
      val index = Integer.parseInt(indexAttr)
      bd.constructorArgs(index) = parsePropertyValue(ele, bd, null)
    } else {
      bd.constructorArgs += parsePropertyValue(ele, bd, null)
    }
  }

  /** Parse a property element. */
  private def parseProperty(ele: Node, bd: Definition): Unit = {
    val propertyName = ele.getAttribute("name", null)
    bd.properties.put(propertyName, parsePropertyValue(ele, bd, propertyName))
  }

  /** Get the value of a property element. May be a list etc. Also used for
   * constructor arguments, "propertyName" being null in this case.
   */
  private def parsePropertyValue(ele: Node, bd: Definition, propertyName: String): Object = {
    val elementName = if (propertyName != null) "<property> element for property '" + propertyName + "'"
    else "<constructor-arg> element"

    val subElement = ele.childNodes.headOption.orNull
    val hasRefAttribute = ele.hasAttribute("ref")
    val hasValueAttribute = ele.hasAttribute("value")
    if ((hasRefAttribute && hasValueAttribute)
      || ((hasRefAttribute || hasValueAttribute) && subElement != null)) {
      error(elementName
        + " is only allowed to contain either 'ref' attribute OR 'value' attribute OR sub-element", ele)
    }

    if (hasRefAttribute) {
      val refName = ele.getAttribute("ref").orNull
      if (!Strings.isNotBlank(refName)) error(elementName + " contains empty 'ref' attribute", ele)
      Reference(refName)
    } else if (hasValueAttribute) {
      val v = ele.getAttribute("value").orNull
      if (null == propertyName && v == "?") {
        InjectPlaceHolder
      } else {
        v
      }
    } else if (subElement != null) {
      parsePropertySubElement(subElement, bd)
    } else {
      // Neither child element nor "ref" or "value" attribute found.
      error(elementName + " must specify a ref or value", ele)
      null
    }
  }

  /** Parse a value, ref or collection sub-element of a property or constructor-arg element.
   */
  private def parsePropertySubElement(ele: Node, bd: Definition): Object = {
    if (ele.name == "bean") {
      parseBeanDefinition(ele, bd)
    } else if (ele.name == "bean") {
      Reference(ele("bean"))
    } else if (ele.name == "value") {
      ele.textContent
    } else if (ele.name == "null") {
      null
    } else if (ele.name == "list") {
      parseList(ele, bd)
    } else if (ele.name == "set") {
      parseSet(ele, bd)
    } else if (ele.name == "map") {
      parseMap(ele, bd)
    } else if (ele.name == "props") {
      parseProps(ele)
    } else {
      error("Unknown property sub-element: [" + ele.name + "]", ele)
    }
  }

  /** Parse a list element. */
  private def parseList(collectionEle: Node, bd: Definition): mutable.Buffer[Object] = {
    val defaultElementType = collectionEle.getAttribute("value-type", null)
    val target = new mutable.ArrayBuffer[Object]
    parseCollection(collectionEle, target, bd, defaultElementType)
    target
  }

  /** Parse a set element. */
  private def parseSet(collectionEle: Node, bd: Definition): mutable.Set[Object] = {
    val defaultElementType = collectionEle.getAttribute("value-type", null)
    val target = new mutable.HashSet[Object]
    parseCollection(collectionEle, target, bd, defaultElementType)
    target
  }

  /** parseCollection. */
  private def parseCollection(collectionEle: Node, target: mutable.Growable[Object], bd: Definition,
                              defaultElementType: String): Unit = {
    collectionEle.childNodes foreach { e => target.addOne(parsePropertySubElement(e, bd)) }
  }

  /** Parse a map element. */
  private def parseMap(mapEle: Node, bd: Definition): collection.Map[Any, Any] = {
    val defaultKeyType = mapEle.getAttribute("key-type", null)
    val defaultValueType = mapEle.getAttribute("value-type", null)

    val map = Collections.newMap[Any, Any]
    (mapEle \ "entry") foreach { entryEle =>
      val keyEle = (entryEle \ "key").headOption.orNull
      val valueEle = (entryEle \ "value").headOption.orNull

      var key: Any = null
      val hasKeyAttribute = entryEle.hasAttribute("key")
      val hasKeyRefAttribute = entryEle.hasAttribute("key-ref")
      if ((hasKeyAttribute && hasKeyRefAttribute) || ((hasKeyAttribute || hasKeyRefAttribute))
        && keyEle != null) {
        error("<entry> element is only allowed to contain either "
          + "a 'key' attribute OR a 'key-ref' attribute OR a <key> sub-element", entryEle)
      }
      if (hasKeyAttribute) {
        key = convertTo(entryEle("key"), defaultKeyType)
      } else if (hasKeyRefAttribute) {
        key = Reference(entryEle("key-ref"))
      } else if (keyEle != null) {
        key = parseKey(keyEle, bd, defaultKeyType)
      } else {
        error("<entry> element must specify a key", entryEle)
      }
      // Extract value from attribute or sub-element.
      var value: Any = null
      val hasValueAttribute = entryEle.hasAttribute("value")
      val hasValueRefAttribute = entryEle.hasAttribute("value-ref")
      if ((hasValueAttribute && hasValueRefAttribute) || (hasValueAttribute || hasValueRefAttribute)
        && valueEle != null) {
        error("<entry> element is only allowed to contain either "
          + "'value' attribute OR 'value-ref' attribute OR <value> sub-element", entryEle)
      }
      if (hasValueAttribute) {
        value = convertTo(entryEle("value"), defaultValueType)
      } else if (hasValueRefAttribute) {
        Reference(entryEle("value-ref"))
      } else if (valueEle != null) {
        value = parsePropertySubElement(valueEle, bd)
      } else {
        error("<entry> element must specify a value", entryEle)
      }
      map.put(key, value)
    }
    map
  }

  /** Parse a key sub-element of a map element. */
  private def parseKey(keyEle: Node, bd: Definition, defaultKeyTypeName: String): Object = {
    parsePropertySubElement(keyEle.childNodes.head, bd)
  }

  /** Parse a props element. */
  private def parseProps(propsEle: Node): java.util.Properties = {
    val props = new Properties()
    (propsEle \ "prop") foreach { propEle => props.put(propEle("key"), propEle.textContent) }
    props
  }

  private def convertTo(v: Any, clazz: String): Any = {
    v match {
      case s: String =>
        if (Strings.isBlank(clazz) || clazz == "java.lang.String" || clazz == "string") v
        else DefaultConversion.Instance.convert(s, ClassLoaders.load(clazz))
      case _ => v
    }
  }
}

/** BeanDefinitionReader
 *
 * @author chaostone
 */
object ReconfigParser {

  /** load bean reconfig.xml */
  def load(url: URL): List[Reconfig.Definition] = {
    val holders = new collection.mutable.ListBuffer[Reconfig.Definition]
    val doc = Document.parse(IOs.readString(url.openStream()))
    val parser = new ReconfigParser()
    doc.childNodes foreach { node =>
      val holder = parser.parseBeanDefinition(node)
      if (null != holder) {
        holders += holder
      }
    }
    holders.toList
  }

  /** Report an error with the given message for the given source element. */
  private def error(message: String, source: Node, cause: Throwable = null): AnyRef = {
    CDILogger.error(message, cause)
    CDILogger.error(source.asInstanceOf[Element].toXml)
    null
  }
}
