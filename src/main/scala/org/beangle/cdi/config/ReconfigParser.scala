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

import org.beangle.cdi.Logger
import org.beangle.commons.cdi.Binder.{InjectPlaceHolder, Reference}
import org.beangle.commons.cdi.Reconfig
import org.beangle.commons.cdi.Reconfig.{Definition, ReconfigType}
import org.beangle.commons.collection.Collections
import org.beangle.commons.conversion.impl.DefaultConversion
import org.beangle.commons.io.IOs
import org.beangle.commons.lang.{ClassLoaders, Strings}
import org.beangle.commons.xml.{Document, Node}

import java.net.URL
import java.util.Properties
import scala.collection.mutable

/** Parser for reconfiguring bean definitions from XML.
 *
 * @author chaostone
 */
class ReconfigParser {

  private val usedNames = new mutable.HashSet[String]

  /** Parse the supplied {{{ <bean> }}} XML element.
   *
   * May return null if errors occur during parsing.
   *
   * @param ele XML node of bean element
   * @return parsed reconfig definition or null
   */
  def parseBeanDefinition(ele: Node): Reconfig.Definition = {
    parseBeanDefinition(ele, null)
  }

  /** Parse the supplied {{{ <bean> }}} element with optional containing bean.
   *
   * @param ele             XML node of bean element
   * @param containingBean parent bean definition if nested, null otherwise
   * @return parsed reconfig definition or null
   */
  private def parseBeanDefinition(ele: Node, containingBean: Definition): Reconfig.Definition = {
    val id = ele.get("id").orNull
    if (null == id) error("missing id", ele)
    if (containingBean == null) checkNameUniqueness(id, ele)
    parseBeanDefinition(ele, id, containingBean)
  }

  /** Validate that the specified bean name has not been used already.
   *
   * @param beanName    bean name to check
   * @param beanElement XML element for error reporting
   */
  private def checkNameUniqueness(beanName: String, beanElement: Node): Unit = {
    if (this.usedNames.contains(beanName)) error("Bean name '" + beanName + "' is already used in this file", beanElement)
    this.usedNames += beanName
  }

  /** Parse the bean definition content, disregarding name or aliases.
   *
   * @param ele              bean XML element
   * @param beanName         bean name from id attribute
   * @param containingBean  parent bean if nested, null otherwise
   * @return parsed definition or null on parse error
   */
  private def parseBeanDefinition(ele: Node, beanName: String, containingBean: Definition): Definition = {
    val className = ele.get("class").orNull
    try {
      val configType = if ele.get("override").contains("remove") then ReconfigType.Remove else ReconfigType.Update
      val bd = new Definition(beanName, configType)
      bd.clazz = Option(if (null == className) null else ClassLoaders.load(className))
      ele.get("primary-of") foreach { p =>
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

  /** Parse constructor-arg sub-elements of the given bean element.
   *
   * @param beanEle bean XML element
   * @param bd      definition to populate
   */
  private def parseConstructorArgs(beanEle: Node, bd: Definition): Unit = {
    (beanEle \ "constructor-arg") foreach { node => parseConstructorArg(node, bd) }
  }

  /** Parse property sub-elements of the given bean element.
   *
   * @param beanEle bean XML element
   * @param bd      definition to populate
   */
  private def parseProperties(beanEle: Node, bd: Definition): Unit = {
    (beanEle \ "property") foreach { node => parseProperty(node, bd) }
  }

  /** Parse a single constructor-arg element.
   *
   * @param ele constructor-arg XML element
   * @param bd  definition to populate
   */
  private def parseConstructorArg(ele: Node, bd: Definition): Unit = {
    val indexAttr = ele.get("index", null)
    if (Strings.isNotEmpty(indexAttr)) {
      val index = Integer.parseInt(indexAttr)
      bd.constructorArgs(index) = parsePropertyValue(ele, bd, null)
    } else {
      bd.constructorArgs += parsePropertyValue(ele, bd, null)
    }
  }

  /** Parse a single property element.
   *
   * @param ele property XML element
   * @param bd  definition to populate
   */
  private def parseProperty(ele: Node, bd: Definition): Unit = {
    val propertyName = ele("name")
    bd.properties.put(propertyName, parsePropertyValue(ele, bd, propertyName))
  }

  /** Get the value of a property or constructor-arg element.
   *
   * May return list, ref, value, etc. For constructor arguments, propertyName is null.
   *
   * @param ele          value/ref/collection element
   * @param bd           definition context
   * @param propertyName property name, or null for constructor-arg
   * @return parsed value
   */
  private def parsePropertyValue(ele: Node, bd: Definition, propertyName: String): Object = {
    val elementName = if (propertyName != null) "<property> element for property '" + propertyName + "'"
    else "<constructor-arg> element"

    val subElement = ele.children.headOption.orNull
    val hasRefAttribute = ele.has("ref")
    val hasValueAttribute = ele.has("value")
    if ((hasRefAttribute && hasValueAttribute)
      || ((hasRefAttribute || hasValueAttribute) && subElement != null)) {
      error(elementName
        + " is only allowed to contain either 'ref' attribute OR 'value' attribute OR sub-element", ele)
    }

    if (hasRefAttribute) {
      val refName = ele.get("ref").orNull
      if (!Strings.isNotBlank(refName)) error(elementName + " contains empty 'ref' attribute", ele)
      Reference(refName)
    } else if (hasValueAttribute) {
      val v = ele.get("value").orNull
      if null == propertyName && v == "?" then InjectPlaceHolder else v
    } else if (subElement != null) {
      parsePropertySubElement(subElement, bd)
    } else {
      error(elementName + " must specify a ref or value", ele)
      null
    }
  }

  /** Parse a value, ref, or collection sub-element of property or constructor-arg.
   *
   * @param ele sub-element (bean, ref, value, null, list, set, map, props)
   * @param bd  definition context
   * @return parsed object
   */
  private def parsePropertySubElement(ele: Node, bd: Definition): Object = {
    ele.label match {
      case "bean" => parseBeanDefinition(ele, bd)
      case "ref" => Reference(ele("bean"))
      case "value" => ele.text
      case "null" => null
      case "list" => parseList(ele, bd)
      case "set" => parseSet(ele, bd)
      case "map" => parseMap(ele, bd)
      case "props" => parseProps(ele)
      case _ => error("Unknown property sub-element: [" + ele.label + "]", ele)
    }
  }

  /** Parse list collection element. */
  private def parseList(collectionEle: Node, bd: Definition): mutable.Buffer[Object] = {
    val target = new mutable.ArrayBuffer[Object]
    parseCollection(collectionEle, target, bd)
    target
  }

  /** Parse set collection element. */
  private def parseSet(collectionEle: Node, bd: Definition): mutable.Set[Object] = {
    val target = new mutable.HashSet[Object]
    parseCollection(collectionEle, target, bd)
    target
  }

  /** Parse collection element and add parsed children to target. */
  private def parseCollection(collectionEle: Node, target: mutable.Growable[Object], bd: Definition): Unit = {
    collectionEle.children foreach { e => target.addOne(parsePropertySubElement(e, bd)) }
  }

  /** Parse map element with entry sub-elements. */
  private def parseMap(mapEle: Node, bd: Definition): collection.Map[Any, Any] = {
    val defaultKeyType = mapEle.get("key-type", null)
    val defaultValueType = mapEle.get("value-type", null)

    val map = Collections.newMap[Any, Any]
    (mapEle \ "entry") foreach { entryEle =>
      val keyEle = (entryEle \ "key").headOption.orNull
      val valueEle = (entryEle \ "value").headOption.orNull

      var key: Any = null
      val hasKeyAttribute = entryEle.has("key")
      val hasKeyRefAttribute = entryEle.has("key-ref")
      if ((hasKeyAttribute && hasKeyRefAttribute) || (hasKeyAttribute || hasKeyRefAttribute) && keyEle != null) {
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
      var value: Any = null
      val hasValueAttribute = entryEle.has("value")
      val hasValueRefAttribute = entryEle.has("value-ref")
      if ((hasValueAttribute && hasValueRefAttribute) || (hasValueAttribute || hasValueRefAttribute) && valueEle != null) {
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

  /** Parse a key sub-element of a map entry element.
   *
   * @param keyEle            key XML element
   * @param bd                definition context
   * @param defaultKeyTypeName default key type for conversion
   * @return parsed key value
   */
  private def parseKey(keyEle: Node, bd: Definition, defaultKeyTypeName: String): Object = {
    parsePropertySubElement(keyEle.children.head, bd)
  }

  /** Parse props element with prop sub-elements. */
  private def parseProps(propsEle: Node): java.util.Properties = {
    val props = new Properties()
    (propsEle \ "prop") foreach { propEle => props.put(propEle("key"), propEle.text) }
    props
  }

  /** Convert string value to target type by class name. */
  private def convertTo(v: Any, clazz: String): Any = {
    v match {
      case s: String =>
        if (Strings.isBlank(clazz) || clazz == "java.lang.String" || clazz == "string") v
        else DefaultConversion.Instance.convert(s, ClassLoaders.load(clazz))
      case _ => v
    }
  }

  /** Report an error with the given message for the given source element.
   *
   * @param message error message
   * @param source  source XML element
   * @param cause   optional exception cause
   * @return always null (for assignment compatibility)
   */
  private def error(message: String, source: Node, cause: Throwable = null): AnyRef = {
    Logger.error(message, cause)
    null
  }
}

/** Loader for bean reconfiguration definitions from XML.
 *
 * @author chaostone
 */
object ReconfigParser {

  /** Load bean reconfig definitions from the given URL.
   *
   * @param url URL to reconfig XML (file, classpath, or http)
   * @return list of parsed reconfig definitions
   */
  def load(url: URL): List[Reconfig.Definition] = {
    val doc = Document.parse(IOs.readString(url.openStream()))
    val parser = new ReconfigParser()
    val holders = new collection.mutable.ListBuffer[Reconfig.Definition]
    doc.children foreach { node =>
      val holder = parser.parseBeanDefinition(node)
      if null != holder then holders += holder
    }
    holders.toList
  }

}
