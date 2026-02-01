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

import org.beangle.cdi.config.ReconfigParser.*
import org.beangle.commons.cdi.Binder.{InjectPlaceHolder, Reference}
import org.beangle.commons.cdi.Reconfig
import org.beangle.commons.cdi.Reconfig.{Definition, ReconfigType}
import org.beangle.commons.collection.Collections
import org.beangle.commons.conversion.impl.DefaultConversion
import org.beangle.commons.lang.{ClassLoaders, Strings}
import org.beangle.commons.logging.Logging
import org.w3c.dom.{CharacterData, Comment, Element, Node}
import org.xml.sax.InputSource

import java.io.FileNotFoundException
import java.net.URL
import java.util.Properties
import javax.xml.parsers.DocumentBuilderFactory
import scala.collection.mutable

/** Reconfig BeanDefinition Parser
 *
 * @author chaostone
 */
class ReconfigParser extends Logging {

  /** Stores all used bean names so we can enforce uniqueness on a per file basis. */
  private val usedNames = new mutable.HashSet[String]

  /** Parses the supplied <code>&ltbean&gt</code> element. May return <code>null</code> if there
   * were errors during parse.
   */
  def parseBeanDefinitionElement(ele: Element): Reconfig.Definition = {
    parseBeanDefinitionElement(ele, null)
  }

  /** Parses the supplied <code>&ltbean&gt</code> element. May return <code>null</code> if there
   * were errors during parse.
   */
  private def parseBeanDefinitionElement(ele: Element, containingBean: Definition): Reconfig.Definition = {
    val id = ele.getAttribute("id")
    if (null == id) error("missing id", ele)
    if (containingBean == null) checkNameUniqueness(id, ele)
    parseBeanDefinitionElement(ele, id, containingBean)
  }

  /** Validate that the specified bean name and aliases have not been used already. */
  private def checkNameUniqueness(beanName: String, beanElement: Element): Unit = {
    if (this.usedNames.contains(beanName)) error("Bean name '" + beanName + "' is already used in this file", beanElement)
    this.usedNames += beanName
  }

  /** Parse the bean definition itself, without regard to name or aliases. May
   * return <code>null</code> if problems occured during the parse of the bean
   * definition.
   */
  private def parseBeanDefinitionElement(ele: Element, beanName: String, containingBean: Definition): Definition = {
    val className = if (ele.hasAttribute("class")) ele.getAttribute("class").trim() else null
    try {
      val ovr = ele.getAttribute("override")
      val configType = if (null != ovr && ovr == "remove") ReconfigType.Remove else ReconfigType.Update
      val bd = new Definition(beanName, configType)
      bd.clazz = Option(if (null == className) null else ClassLoaders.load(className))
      val primary = ele.getAttribute("primary-of")
      if (Strings.isNotEmpty(primary)) {
        bd.primaryOf(Strings.split(primary).map(ClassLoaders.load(_)): _*)
      }
      parseConstructorArgElements(ele, bd)
      parsePropertyElements(ele, bd)
      return bd
    } catch {
      case ex: ClassNotFoundException => error("Bean class [" + className + "] not found", ele, ex)
      case exr: Throwable => error("Unexpected failure during bean definition parsing", ele, exr)
    }
    null
  }

  /** Parse constructor-arg sub-elements of the given bean element. */
  private def parseConstructorArgElements(beanEle: Element, bd: Definition): Unit = {
    childrenOf(beanEle, "constructor-arg") foreach { node =>
      parseConstructorArgElement(node, bd)
    }
  }

  /** Parse property sub-elements of the given bean element. */
  private def parsePropertyElements(beanEle: Element, bd: Definition): Unit = {
    childrenOf(beanEle, PROPERTY) foreach { node =>
      parsePropertyElement(node, bd)
    }
  }

  /** Parse a constructor-arg element. */
  private def parseConstructorArgElement(ele: Element, bd: Definition): Unit = {
    val indexAttr = ele.getAttribute(INDEX)
    if (bd.constructorArgs == null) {
      bd.constructorArgs = new mutable.ArrayBuffer[Any]
    }
    if (Strings.isNotEmpty(indexAttr)) {
      val index = Integer.parseInt(indexAttr)
      bd.constructorArgs(index) = parsePropertyValue(ele, bd, null)
    } else {
      bd.constructorArgs += parsePropertyValue(ele, bd, null)
    }
  }

  /** Parse a property element. */
  private def parsePropertyElement(ele: Element, bd: Definition): Unit = {
    val propertyName = ele.getAttribute("name")
    bd.properties.put(propertyName, parsePropertyValue(ele, bd, propertyName))
  }

  /** Get the value of a property element. May be a list etc. Also used for
   * constructor arguments, "propertyName" being null in this case.
   */
  private def parsePropertyValue(ele: Element, bd: Definition, propertyName: String): Object = {
    val elementName = if (propertyName != null) "<property> element for property '" + propertyName + "'"
    else "<constructor-arg> element"

    val subElement = childOf(ele)

    val hasRefAttribute = ele.hasAttribute(REF)
    val hasValueAttribute = ele.hasAttribute(VALUE)
    if ((hasRefAttribute && hasValueAttribute)
      || ((hasRefAttribute || hasValueAttribute) && subElement != null)) {
      error(elementName
        + " is only allowed to contain either 'ref' attribute OR 'value' attribute OR sub-element", ele)
    }

    if (hasRefAttribute) {
      val refName = ele.getAttribute(REF)
      if (!Strings.isNotBlank(refName)) error(elementName + " contains empty 'ref' attribute", ele)
      Reference(refName)
    } else if (hasValueAttribute) {
      val v = ele.getAttribute(VALUE)
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
  private def parsePropertySubElement(ele: Element, bd: Definition): Object = {
    if (nodeNameEquals(ele, BEAN)) {
      parseBeanDefinitionElement(ele, bd)
    } else if (nodeNameEquals(ele, REF)) {
      Reference(ele.getAttribute(BEAN_REF))
    } else if (nodeNameEquals(ele, VALUE)) {
      getTextValue(ele)
    } else if (nodeNameEquals(ele, "null")) {
      null
    } else if (nodeNameEquals(ele, LIST)) {
      parseListElement(ele, bd)
    } else if (nodeNameEquals(ele, SET)) {
      parseSetElement(ele, bd)
    } else if (nodeNameEquals(ele, MAP)) {
      parseMapElement(ele, bd)
    } else if (nodeNameEquals(ele, "props")) {
      parsePropsElement(ele)
    } else {
      error("Unknown property sub-element: [" + ele.getNodeName + "]", ele)
      null
    }
  }

  /** Parse a list element. */
  private def parseListElement(collectionEle: Element, bd: Definition): mutable.Buffer[Object] = {
    val defaultElementType = collectionEle.getAttribute("value-type")
    val target = new mutable.ArrayBuffer[Object]
    parseCollectionElements(collectionEle, target, bd, defaultElementType)
    target
  }

  /** Parse a set element. */
  private def parseSetElement(collectionEle: Element, bd: Definition): mutable.Set[Object] = {
    val defaultElementType = collectionEle.getAttribute("value-type")
    val target = new mutable.HashSet[Object]
    parseCollectionElements(collectionEle, target, bd, defaultElementType)
    target
  }


  /** parseCollectionElements. */
  private def parseCollectionElements(collectionEle: Element, target: mutable.Growable[Object], bd: Definition,
                                      defaultElementType: String): Unit = {
    childrenOf(collectionEle) foreach { e =>
      target.addOne(parsePropertySubElement(e, bd))
    }
  }

  /** Parse a map element. */
  private def parseMapElement(mapEle: Element, bd: Definition): collection.Map[Any, Any] = {
    val defaultKeyType = mapEle.getAttribute("key-type")
    val defaultValueType = mapEle.getAttribute("value-type")

    val map = Collections.newMap[Any, Any]
    childrenOf(mapEle, "entry") foreach { entryEle =>
      val keyEle = childOf(entryEle, KEY)
      val valueEle = childOf(entryEle, VALUE)

      var key: Any = null
      val hasKeyAttribute = entryEle.hasAttribute(KEY)
      val hasKeyRefAttribute = entryEle.hasAttribute("key-ref")
      if ((hasKeyAttribute && hasKeyRefAttribute) || ((hasKeyAttribute || hasKeyRefAttribute))
        && keyEle != null) {
        error("<entry> element is only allowed to contain either "
          + "a 'key' attribute OR a 'key-ref' attribute OR a <key> sub-element", entryEle)
      }
      if (hasKeyAttribute) {
        key = convertTo(entryEle.getAttribute(KEY), defaultKeyType)
      } else if (hasKeyRefAttribute) {
        key = Reference(entryEle.getAttribute("key-ref"))
      } else if (keyEle != null) {
        key = parseKeyElement(keyEle, bd, defaultKeyType)
      } else {
        error("<entry> element must specify a key", entryEle)
      }
      // Extract value from attribute or sub-element.
      var value: Any = null
      val hasValueAttribute = entryEle.hasAttribute(VALUE)
      val hasValueRefAttribute = entryEle.hasAttribute("value-ref")
      if ((hasValueAttribute && hasValueRefAttribute) || (hasValueAttribute || hasValueRefAttribute)
        && valueEle != null) {
        error("<entry> element is only allowed to contain either "
          + "'value' attribute OR 'value-ref' attribute OR <value> sub-element", entryEle)
      }
      if (hasValueAttribute) {
        value = convertTo(entryEle.getAttribute(VALUE), defaultValueType)
      } else if (hasValueRefAttribute) {
        Reference(entryEle.getAttribute("value-ref"))
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
  private def parseKeyElement(keyEle: Element, bd: Definition, defaultKeyTypeName: String): Object = {
    parsePropertySubElement(childOf(keyEle), bd)
  }

  /** Parse a props element. */
  private def parsePropsElement(propsEle: Element): java.util.Properties = {
    val props = new Properties()
    childrenOf(propsEle, "prop") foreach { propEle =>
      props.put(propEle.getAttribute(KEY), getTextValue(propEle))
    }
    props
  }

  private def nodeNameEquals(node: Node, desiredName: String): Boolean = {
    desiredName.equals(node.getNodeName) || desiredName.equals(node.getLocalName)
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
object ReconfigParser extends Logging {

  /** load bean reconfig.xml */
  def load(url: URL): List[Reconfig.Definition] = {
    val holders = new collection.mutable.ListBuffer[Reconfig.Definition]
    try {
      val inputStream = url.openStream
      try {
        val inputSource = new InputSource(inputStream)
        val factory = DocumentBuilderFactory.newInstance()
        factory.setNamespaceAware(false)
        val docBuilder = factory.newDocumentBuilder()
        val doc = docBuilder.parse(inputSource)
        val parser = new ReconfigParser()
        val root = doc.getDocumentElement
        childrenOf(root, BEAN) foreach { ele =>
          val holder = parser.parseBeanDefinitionElement(ele)
          if (null != holder) {
            holders += holder
          }
        }
      } finally {
        if (null != inputStream) inputStream.close()
      }
    } catch {
      case _: FileNotFoundException => //ignore
      case ex: Exception => throw new RuntimeException("IOException parsing XML document from " + url, ex)
    }
    holders.toList
  }

  private def childrenOf(ele: Element, tagName: String = null): Iterable[Element] = {
    val nl = ele.getChildNodes
    val elems = Collections.newBuffer[Element]
    for (i <- 0 until nl.getLength) {
      nl.item(i) match {
        case e: Element =>
          if "description" != e.getNodeName && (tagName == null || e.getNodeName == tagName) then elems.addOne(e)
        case _ =>
      }
    }
    elems
  }

  private def childOf(ele: Element, tagName: String = null): Element = {
    val nl = ele.getChildNodes
    var childEle: Element = null
    for (i <- 0 until nl.getLength) {
      val node = nl.item(i)
      nl.item(i) match {
        case e: Element =>
          if (tagName == null && node.getNodeName != "description"
            && node.getNodeName != "meta" || node.getNodeName == tagName) {
            if (childEle != null) error(s"<${ele.getNodeName}> only one <${tagName}> sub-element needed", ele)
            else childEle = e
          }
        case _ =>
      }
    }
    childEle
  }

  /** Report an error with the given message for the given source element. */
  private def error(message: String, source: Any, cause: Throwable = null): Any = {
    logger.error(message)
    null
  }

  private def getTextValue(valueEle: Element): String = {
    val sb = new StringBuilder
    val nl = valueEle.getChildNodes
    for (i <- 0 until nl.getLength) {
      val item = nl.item(i)
      if (item.isInstanceOf[CharacterData] && !item.isInstanceOf[Comment]) sb.append(item.getNodeValue)
    }
    sb.toString.trim()
  }

  val PROPERTY = "property"
  val INDEX = "index"
  val REF = "ref"
  val VALUE = "value"
  val BEAN = "bean"
  val LIST = "list"
  val SET = "set"
  val MAP = "map"
  val KEY = "key"
  val BEAN_REF = "bean"
}
