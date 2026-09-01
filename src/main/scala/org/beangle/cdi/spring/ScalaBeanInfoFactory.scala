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

package org.beangle.cdi.spring

import org.beangle.commons.lang.reflect.BeanInfos
import org.springframework.beans.BeanInfoFactory

import java.beans.{BeanInfo, PropertyDescriptor}

/** BeanInfoFactory registered via SpringFactoriesLoader.
 *
 * Replaces the default JDK BeanInfoFactory so that Spring's
 * CachedIntrospectionResults delegates to [[ScalaBeanInfo]] for every
 * non-JDK, non-interface class. This keeps the introspection cache small
 * and avoids the expensive `Introspector.getBeanInfo` call that would
 * otherwise descend into all superclass / interface hierarchies.
 *
 * Discovered via `META-INF/spring.factories` (Spring 7 convention) and
 * `META-INF/beangle/aot-registrars.txt` (GraalVM native-image hints).
 */
class ScalaBeanInfoFactory extends BeanInfoFactory {

  /** Return ScalaBeanInfo for custom classes, empty ScalaBeanInfo for java.* / scala.* / interfaces.
   *
   * @param beanClass class to introspect
   * @return BeanInfo (never null)
   */
  def getBeanInfo(beanClass: Class[_]): BeanInfo = {
    val className = beanClass.getName
    // JDK / Scala 标准库类型以及接口无需构建属性描述符，直接返回空 BeanInfo
    if (className.startsWith("java.") || className.startsWith("scala.") || beanClass.isInterface) {
      new ScalaBeanInfo(beanClass, Array.empty)
    } else {
      new ScalaBeanInfo(beanClass, buildProperties(beanClass))
    }
  }

  /** Build PropertyDescriptor array from beangle-commons BeanInfos manifest. */
  private def buildProperties(beanClass: Class[_]): Array[PropertyDescriptor] = {
    val descriptors = new collection.mutable.HashMap[String, PropertyDescriptor]
    val manifest = BeanInfos.get(beanClass)
    for ((name, mi) <- manifest.properties) {
      descriptors.put(name, new PropertyDescriptor(name, manifest.getGetterMethod(name).orNull, manifest.getSetterMethod(name).orNull))
    }
    // write-only 属性（仅 setter、无 getter，如 Spring AbstractSingletonProxyFactoryBean
    // 的 setProxyInterfaces）由 BeanInfo.writeOnlys 单独提供，这里补成可写描述符。
    for ((name, setter) <- manifest.writeOnlys) {
      descriptors.put(name, new PropertyDescriptor(name, null, setter))
    }
    descriptors.values.toArray
  }
}
