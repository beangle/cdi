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

package org.beangle.cdi.aot

import org.beangle.cdi.config.ContainerEventMulticaster
import org.beangle.cdi.spring.{FactoryBeanProxy, ScalaBeanInfo, ScalaBeanInfoFactory}
import org.beangle.commons.aot.{AotHintRegistrar, AotPolicy}

/** beangle-cdi 的 GraalVM native-image 提示。
  *
  * - `META-INF/spring.factories`：Spring 7 的 `CachedIntrospectionResults` 经
  *   `SpringFactoriesLoader`（classloader.getResources）发现 `BeanInfoFactory`
  *   实现；native 镜像必须注册该资源，否则回退 JDK `Introspector`，Scala 属性
  *   （如 `var registry` 的 `registry_$eq` setter）无法识别。
  * - `ScalaBeanInfoFactory`/`ScalaBeanInfo`：工厂经 `Class.forName` + 构造器反射
  *   实例化，BeanInfo 内省结果由 Spring 反射消费。
  * - cdi 内置 bean（`FactoryBeanProxy` 包装类、`ContainerEventMulticaster`）：Spring
  *   反射实例化（public 构造器）并经 BeanWrapper 设置 Scala `var` 属性。它们无预编译
  *   beanmeta，运行期经 `MetaLoader` 反射 dig，属性识别依赖 `getDeclaredFields`
  *   （私有字段）与 `getDeclaredMethods`，因此按 declared 成员注册（默认策略只有
  *   public 成员）。
  */
class SpringAotHints extends AotHintRegistrar {

  /** Register GraalVM native-image reflection and resource hints. */
  override def registering(): Unit = {
    // Spring 7 通过 SpringFactoriesLoader 发现 BeanInfoFactory 实现，
    // native 镜像必须注册 META-INF/spring.factories 资源文件
    hints.registerPattern("META-INF/spring.factories")
    // ScalaBeanInfoFactory/ScalaBeanInfo 由 Spring 通过反射实例化
    hints.registerType(classOf[ScalaBeanInfoFactory], classOf[ScalaBeanInfo])
    // FactoryBeanProxy / ContainerEventMulticaster：Spring 通过反射实例化并设置属性，
    // BeanInfo 内省依赖 getDeclaredFields/getDeclaredMethods，因此按 declared 成员注册
    hints.registerType(classOf[FactoryBeanProxy[_]])
    hints.registerType(classOf[ContainerEventMulticaster])
    // Spring 核心 ResolvableType 缓存经 SerializableTypeWrapper 对
    // GenericArrayType/ParameterizedType/TypeVariable 创建 JDK 动态代理。
    // SerializableTypeWrapper 为 private[core]，无法直接引用，
    // 按接口名注册，顺序与运行期一致（代理类按接口列表缓存）
    hints.registerProxyByName(
      "java.lang.reflect.GenericArrayType",
      "org.springframework.core.SerializableTypeWrapper$SerializableTypeProxy",
      "java.io.Serializable")
    hints.registerProxyByName(
      "java.lang.reflect.ParameterizedType",
      "org.springframework.core.SerializableTypeWrapper$SerializableTypeProxy",
      "java.io.Serializable")
    hints.registerProxyByName(
      "java.lang.reflect.TypeVariable",
      "org.springframework.core.SerializableTypeWrapper$SerializableTypeProxy",
      "java.io.Serializable")
  }
}
