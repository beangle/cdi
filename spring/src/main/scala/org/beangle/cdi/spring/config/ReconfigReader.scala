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

import org.beangle.cdi.bind.Reconfig
import org.springframework.core.io.Resource
import org.w3c.dom.Element
import org.xml.sax.InputSource

import java.io.FileNotFoundException
import javax.xml.parsers.DocumentBuilderFactory

/** BeanDefinitionReader
 *
 * @author chaostone
 */
object ReconfigReader {

  /** load spring-config.xml
   */
  def load(resource: Resource): List[Reconfig.Definition] = {
    val holders = new collection.mutable.ListBuffer[Reconfig.Definition]
    try {
      val inputStream = resource.getInputStream
      try {
        val inputSource = new InputSource(inputStream)
        val factory = DocumentBuilderFactory.newInstance()
        val docBuilder = factory.newDocumentBuilder()
        val doc = docBuilder.parse(inputSource)
        val root = doc.getDocumentElement
        val nl = root.getChildNodes
        val parser = new ReconfigParser()
        for (i <- 0 until nl.getLength) {
          val node = nl.item(i)
          if (node.isInstanceOf[Element]) {
            val ele = node.asInstanceOf[Element]
            val holder = parser.parseBeanDefinitionElement(ele)
            if (null != holder) {
              holders += holder
            }
          }
        }
      } finally {
        if (null != inputStream) inputStream.close()
      }
    } catch {
      case _: FileNotFoundException => //ignore
      case ex: Exception => throw new RuntimeException("IOException parsing XML document from " + resource.getDescription(), ex)
    }
    holders.toList
  }

}
