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

import org.beangle.cdi.bind.Binding.InjectPlaceHolder
import org.beangle.cdi.bind.Reconfig.ReconfigType
import org.beangle.cdi.bind.{Binding, Reconfig}
import org.beangle.commons.collection.Collections
import org.beangle.commons.conversion.impl.DefaultConversion
import org.beangle.commons.lang.{ClassLoaders, Strings}
import org.beangle.commons.logging.Logging
import org.springframework.beans.factory.support.{ManagedArray, ManagedList, ManagedSet}
import org.springframework.beans.factory.xml.BeanDefinitionParserDelegate._
import org.springframework.util.StringUtils
import org.springframework.util.xml.DomUtils
import org.w3c.dom.{Element, Node, NodeList}

import java.util.Properties
import scala.collection.mutable

/**
 * Reconfig BeanDefinition Parser
 *
 * @author chaostone
 */
class ReconfigParser extends Logging {

  /**
   * Stores all used bean names so we can enforce uniqueness on a per file
   * basis.
   */
  private val usedNames = new mutable.HashSet[String]

  /**
   * Report an error with the given message for the given source element.
   */
  protected def error(message: String, source: Any, cause: Throwable = null): Unit = {
    logger.error(message)
  }

  /**
   * Parses the supplied <code>&ltbean&gt</code> element. May return <code>null</code> if there
   * were errors during parse.
   */
  def parseBeanDefinitionElement(ele: Element): Reconfig.Definition = {
    parseBeanDefinitionElement(ele, null)
  }

  /**
   * Parses the supplied <code>&ltbean&gt</code> element. May return <code>null</code> if there
   * were errors during parse.
   */
  private def parseBeanDefinitionElement(ele: Element, containingBean: Binding.Definition): Reconfig.Definition = {
    val id = ele.getAttribute(ID_ATTRIBUTE)
    val nameAttr = ele.getAttribute(NAME_ATTRIBUTE)

    val aliases = new mutable.ListBuffer[String]
    if (Strings.isNotEmpty(nameAttr)) {
      val nameArr = StringUtils.tokenizeToStringArray(nameAttr, MULTI_VALUE_ATTRIBUTE_DELIMITERS)
      for (name <- nameArr) aliases += name
    }

    var beanName = id
    if (!StringUtils.hasText(beanName) && !aliases.isEmpty) {
      beanName = aliases.remove(0)
      logger.debug(s"No XML 'id' specified - using '$beanName' as bean name and $aliases as aliases")
    }

    if (containingBean == null) checkNameUniqueness(beanName, aliases, ele)

    val beanDefinition = parseBeanDefinitionElement(ele, beanName, containingBean)
    if (beanDefinition != null) {
      val ovr = ele.getAttribute("override")
      val configType =
        if (null != ovr && ovr == "remove") ReconfigType.Remove
        else {
          val primary = ele.getAttribute("primary")
          if (null != primary && primary.equals("true")) ReconfigType.Primary
          else ReconfigType.Update
        }
      return new Reconfig.Definition(beanName, configType, beanDefinition)
    }
    null
  }

  /**
   * Validate that the specified bean name and aliases have not been used
   * already.
   */
  protected def checkNameUniqueness(beanName: String, aliases: collection.Seq[String], beanElement: Element): Unit = {
    var foundName: String = null
    if (StringUtils.hasText(beanName) && this.usedNames.contains(beanName)) foundName = beanName
    if (foundName == null) foundName = Collections.findFirstMatch(this.usedNames, aliases).orNull
    if (foundName != null) error("Bean name '" + foundName + "' is already used in this file", beanElement)
    this.usedNames += beanName
    this.usedNames ++= aliases
  }

  /**
   * Parse the bean definition itself, without regard to name or aliases. May
   * return <code>null</code> if problems occured during the parse of the bean
   * definition.
   */
  private def parseBeanDefinitionElement(ele: Element, beanName: String, containingBean: Binding.Definition): Binding.Definition = {
    val className = if (ele.hasAttribute(CLASS_ATTRIBUTE)) ele.getAttribute(CLASS_ATTRIBUTE).trim() else null
    try {
      val bd = new Binding.Definition(beanName, if (null == className) null else ClassLoaders.load(className), null)
      parseBeanDefinitionAttributes(ele, beanName, containingBean, bd)
      parseConstructorArgElements(ele, bd)
      parsePropertyElements(ele, bd)
      return bd
    } catch {
      case ex: ClassNotFoundException => error("Bean class [" + className + "] not found", ele, ex)
      case exr: Throwable => error("Unexpected failure during bean definition parsing", ele, exr)
    }
    null
  }

  /**
   * Apply the attributes of the given bean element to the given bean *
   * definition.
   */
  private def parseBeanDefinitionAttributes(ele: Element, beanName: String,
                                            containingBean: Binding.Definition, bd: Binding.Definition): Binding.Definition = {
    if (ele.hasAttribute(SCOPE_ATTRIBUTE)) {
      bd.scope = ele.getAttribute(SCOPE_ATTRIBUTE)
    } else if (containingBean != null) {
      bd.scope = containingBean.scope
    } else {
      bd.scope = "singleton"
    }

    if (ele.hasAttribute(ABSTRACT_ATTRIBUTE))
      bd.abstractFlag = TRUE_VALUE == ele.getAttribute(ABSTRACT_ATTRIBUTE)

    bd.lazyInit = TRUE_VALUE == ele.getAttribute(LAZY_INIT_ATTRIBUTE)

    if (ele.hasAttribute(PRIMARY_ATTRIBUTE))
      bd.primary = TRUE_VALUE == ele.getAttribute(PRIMARY_ATTRIBUTE)

    if (ele.hasAttribute(INIT_METHOD_ATTRIBUTE)) {
      val initMethodName = ele.getAttribute(INIT_METHOD_ATTRIBUTE)
      if (!"".equals(initMethodName)) bd.initMethod = initMethodName
    }

    if (ele.hasAttribute(DESTROY_METHOD_ATTRIBUTE)) {
      val destroyMethodName = ele.getAttribute(DESTROY_METHOD_ATTRIBUTE)
      if (!"".equals(destroyMethodName)) bd.destroyMethod = destroyMethodName
    }

    if (ele.hasAttribute(FACTORY_METHOD_ATTRIBUTE))
      bd.factoryMethod = ele.getAttribute(FACTORY_METHOD_ATTRIBUTE)
    if (ele.hasAttribute(FACTORY_BEAN_ATTRIBUTE))
      bd.factoryBean = ele.getAttribute(FACTORY_BEAN_ATTRIBUTE)

    bd
  }

  /**
   * Parse constructor-arg sub-elements of the given bean element.
   */
  private def parseConstructorArgElements(beanEle: Element, bd: Binding.Definition): Unit = {
    findChildren(beanEle, CONSTRUCTOR_ARG_ELEMENT) foreach { node =>
      parseConstructorArgElement(node, bd)
    }
  }

  /**
   * Parse property sub-elements of the given bean element.
   *
   */
  private def parsePropertyElements(beanEle: Element, bd: Binding.Definition): Unit = {
    findChildren(beanEle, PROPERTY_ELEMENT) foreach { node =>
      parsePropertyElement(node, bd)
    }
  }

  /**
   * Parse a constructor-arg element.
   *
   */
  private def parseConstructorArgElement(ele: Element, bd: Binding.Definition): Unit = {
    val indexAttr = ele.getAttribute(INDEX_ATTRIBUTE)
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

  /**
   * Parse a property element.
   *
   */
  private def parsePropertyElement(ele: Element, bd: Binding.Definition): Unit = {
    val propertyName = ele.getAttribute(NAME_ATTRIBUTE)
    bd.properties.put(propertyName, parsePropertyValue(ele, bd, propertyName))
  }

  /**
   * Get the value of a property element. May be a list etc. Also used for
   * constructor arguments, "propertyName" being null in this case.
   */
  private def parsePropertyValue(ele: Element, bd: Binding.Definition, propertyName: String): Object = {
    val elementName = if (propertyName != null) "<property> element for property '" + propertyName + "'"
    else "<constructor-arg> element"

    val subElement = findOnlyOneChildren(ele)

    val hasRefAttribute = ele.hasAttribute(REF_ATTRIBUTE)
    val hasValueAttribute = ele.hasAttribute(VALUE_ATTRIBUTE)
    if ((hasRefAttribute && hasValueAttribute)
      || ((hasRefAttribute || hasValueAttribute) && subElement != null)) {
      error(elementName
        + " is only allowed to contain either 'ref' attribute OR 'value' attribute OR sub-element", ele)
    }

    if (hasRefAttribute) {
      val refName = ele.getAttribute(REF_ATTRIBUTE)
      if (!StringUtils.hasText(refName)) error(elementName + " contains empty 'ref' attribute", ele)
      Binding.ReferenceValue(refName)
    } else if (hasValueAttribute) {
      val v = ele.getAttribute(VALUE_ATTRIBUTE)
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

  /**
   * <p>
   * parsePropertySubElement.
   * </p>
   *
   */
  private def parsePropertySubElement(ele: Element, bd: Binding.Definition): Object = {
    parsePropertySubElement(ele, bd, null)
  }

  /**
   * Parse a value, ref or collection sub-element of a property or
   * constructor-arg element.
   */
  private def parsePropertySubElement(ele: Element, bd: Binding.Definition, defaultValueType: String): Object = {
    if (!isDefaultNamespace(ele.getNamespaceURI)) {
      error("Cannot support nested element .", ele)
      null
    } else if (nodeNameEquals(ele, BEAN_ELEMENT)) {
      parseBeanDefinitionElement(ele, bd)
    } else if (nodeNameEquals(ele, REF_ELEMENT)) {
      Binding.ReferenceValue(ele.getAttribute(BEAN_REF_ATTRIBUTE))
    } else if (nodeNameEquals(ele, IDREF_ELEMENT)) {
      Binding.ReferenceValue(ele.getAttribute(BEAN_REF_ATTRIBUTE))
    } else if (nodeNameEquals(ele, VALUE_ELEMENT)) {
      parseValueElement(ele, defaultValueType)
    } else if (nodeNameEquals(ele, NULL_ELEMENT)) {
      null
    } else if (nodeNameEquals(ele, ARRAY_ELEMENT)) {
      parseArrayElement(ele, bd)
    } else if (nodeNameEquals(ele, LIST_ELEMENT)) {
      parseListElement(ele, bd)
    } else if (nodeNameEquals(ele, SET_ELEMENT)) {
      parseSetElement(ele, bd)
    } else if (nodeNameEquals(ele, MAP_ELEMENT)) {
      parseMapElement(ele, bd)
    } else if (nodeNameEquals(ele, PROPS_ELEMENT)) {
      parsePropsElement(ele)
    } else {
      error("Unknown property sub-element: [" + ele.getNodeName() + "]", ele)
      null
    }
  }

  /**
   * Return a typed String value Object for the given value element.
   */
  private def parseValueElement(ele: Element, defaultTypeName: String): Object = {
    DomUtils.getTextValue(ele) // It's a literal value.
  }

  /**
   * Parse an array element.
   */
  private def parseArrayElement(arrayEle: Element, bd: Binding.Definition): Object = {
    val elementType = arrayEle.getAttribute(VALUE_TYPE_ATTRIBUTE)
    val nl = arrayEle.getChildNodes
    val target = new ManagedArray(elementType, nl.getLength())
    target.setElementTypeName(elementType)
    parseCollectionElements(nl, target, bd, elementType)
    target
  }

  /**
   * Parse a list element.
   */
  private def parseListElement(collectionEle: Element, bd: Binding.Definition): java.util.List[Object] = {
    val defaultElementType = collectionEle.getAttribute(VALUE_TYPE_ATTRIBUTE)
    val nl = collectionEle.getChildNodes()
    val target = new ManagedList[Object](nl.getLength())
    target.setElementTypeName(defaultElementType)
    parseCollectionElements(nl, target, bd, defaultElementType)
    target
  }

  /**
   * Parse a set element.
   */
  private def parseSetElement(collectionEle: Element, bd: Binding.Definition): java.util.Set[Object] = {
    val defaultElementType = collectionEle.getAttribute(VALUE_TYPE_ATTRIBUTE)
    val nl = collectionEle.getChildNodes()
    val target = new ManagedSet[Object](nl.getLength())
    target.setElementTypeName(defaultElementType)
    parseCollectionElements(nl, target, bd, defaultElementType)
    target
  }

  /**
   * parseCollectionElements.
   */
  protected def parseCollectionElements(elementNodes: NodeList, target: java.util.Collection[Object], bd: Binding.Definition,
                                        defaultElementType: String): Unit = {
    for (i <- 0 until elementNodes.getLength) {
      val node = elementNodes.item(i)
      if (node.isInstanceOf[Element] && !nodeNameEquals(node, DESCRIPTION_ELEMENT))
        target.add(parsePropertySubElement(node.asInstanceOf[Element], bd, defaultElementType))
    }
  }


  /**
   * Parse a map element.
   */
  private def parseMapElement(mapEle: Element, bd: Binding.Definition): collection.Map[Any, Any] = {
    val defaultKeyType = mapEle.getAttribute(KEY_TYPE_ATTRIBUTE)
    val defaultValueType = mapEle.getAttribute(VALUE_TYPE_ATTRIBUTE)

    val map = Collections.newMap[Any, Any]
    findChildren(mapEle, ENTRY_ELEMENT) foreach { entryEle =>
      val keyEle = findOnlyOneChildren(entryEle, KEY_ELEMENT)
      val valueEle = findOnlyOneChildren(entryEle, VALUE_ELEMENT)

      var key: Any = null
      val hasKeyAttribute = entryEle.hasAttribute(KEY_ATTRIBUTE)
      val hasKeyRefAttribute = entryEle.hasAttribute(KEY_REF_ATTRIBUTE)
      if ((hasKeyAttribute && hasKeyRefAttribute) || ((hasKeyAttribute || hasKeyRefAttribute))
        && keyEle != null) {
        error("<entry> element is only allowed to contain either "
          + "a 'key' attribute OR a 'key-ref' attribute OR a <key> sub-element", entryEle)
      }
      if (hasKeyAttribute) {
        key = convertTo(entryEle.getAttribute(KEY_ATTRIBUTE), defaultKeyType)
      } else if (hasKeyRefAttribute) {
        key = Binding.ReferenceValue(entryEle.getAttribute(KEY_REF_ATTRIBUTE))
      } else if (keyEle != null) {
        key = parseKeyElement(keyEle, bd, defaultKeyType)
      } else {
        error("<entry> element must specify a key", entryEle)
      }

      // Extract value from attribute or sub-element.
      var value: Any = null
      val hasValueAttribute = entryEle.hasAttribute(VALUE_ATTRIBUTE)
      val hasValueRefAttribute = entryEle.hasAttribute(VALUE_REF_ATTRIBUTE)
      if ((hasValueAttribute && hasValueRefAttribute) || (hasValueAttribute || hasValueRefAttribute)
        && valueEle != null) {
        error("<entry> element is only allowed to contain either "
          + "'value' attribute OR 'value-ref' attribute OR <value> sub-element", entryEle)
      }
      if (hasValueAttribute) {
        value = convertTo(entryEle.getAttribute(VALUE_ATTRIBUTE), defaultValueType)
      } else if (hasValueRefAttribute) {
        Binding.ReferenceValue(entryEle.getAttribute(VALUE_REF_ATTRIBUTE))
      } else if (valueEle != null) {
        value = parsePropertySubElement(valueEle, bd, defaultValueType)
      } else {
        error("<entry> element must specify a value", entryEle)
      }
      map.put(key, value)
    }

    map
  }

  /**
   * Parse a key sub-element of a map element.
   */
  protected def parseKeyElement(keyEle: Element, bd: Binding.Definition, defaultKeyTypeName: String): Object = {
    parsePropertySubElement(findOnlyOneChildren(keyEle), bd, defaultKeyTypeName)
  }

  /**
   * Parse a props element.
   */
  private def parsePropsElement(propsEle: Element): java.util.Properties = {
    val props = new Properties()
    findChildren(propsEle, PROP_ELEMENT) foreach { propEle =>
      val key = propEle.getAttribute(KEY_ATTRIBUTE)
      val value = DomUtils.getTextValue(propEle).trim()
      props.put(key, value)
    }
    props
  }

  private def isDefaultNamespace(namespaceUri: String): Boolean = Strings.isEmpty(namespaceUri) || BEANS_NAMESPACE_URI == namespaceUri

  private def nodeNameEquals(node: Node, desiredName: String): Boolean = {
    desiredName.equals(node.getNodeName) || desiredName.equals(node.getLocalName)
  }

  private def findChildren(ele: Element, tagName: String): Iterable[Element] = {
    val children = Collections.newBuffer[Element]
    val entrySubNodes = ele.getChildNodes
    for (j <- 0 until entrySubNodes.getLength) {
      val node = entrySubNodes.item(j)
      if (node.isInstanceOf[Element]) {
        val candidateEle = node.asInstanceOf[Element]
        if (nodeNameEquals(candidateEle, tagName)) {
          children += candidateEle
        }
      }
    }
    children
  }

  private def findOnlyOneChildren(ele: Element, tagName: String = null): Element = {
    val entrySubNodes = ele.getChildNodes()
    var childEle: Element = null
    for (j <- 0 until entrySubNodes.getLength) {
      val node = entrySubNodes.item(j)
      if (node.isInstanceOf[Element]) {
        val candidateEle = node.asInstanceOf[Element]
        if (tagName == null && !nodeNameEquals(node, DESCRIPTION_ELEMENT)
          && !nodeNameEquals(node, META_ELEMENT) || nodeNameEquals(candidateEle, tagName)) {
          if (childEle != null) error(s"<${ele.getNodeName}> only one <${tagName}> sub-element needed", ele)
          else childEle = candidateEle
        }
      }
    }
    childEle
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
