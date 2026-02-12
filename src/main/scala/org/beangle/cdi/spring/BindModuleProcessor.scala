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

import org.beangle.cdi.Logger
import org.beangle.cdi.config.{BindingLoader, BindingRegistry, ContainerEventMulticaster}
import org.beangle.commons.cdi.{Binder, Condition}
import org.beangle.commons.lang.time.Stopwatch
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory
import org.springframework.beans.factory.support.*
import org.springframework.util.ClassUtils

/** Processor for automatic bean registration and reconfiguration.
 *
 * Extends BeanDefinitionRegistryPostProcessor and executes early in the container lifecycle.
 *
 * @author chaostone
 */
abstract class BindModuleProcessor(private val configLocation: String) extends BeanDefinitionRegistryPostProcessor {

  /** Automatically register, wire, and reconfig beans from bind modules. */
  override def postProcessBeanDefinitionRegistry(springRegistry: BeanDefinitionRegistry): Unit = {
    val watch = Stopwatch.start()
    val (modules, reconfigs) = BindingLoader.loadModules(configLocation)
    val items = BindingLoader.loadRegistryItems(modules)
    val existed = SpringBeanRegistry.findBeans(springRegistry)
    Logger.info(s"Load ${items.size} beans in $watch")

    //reconfig autowire and register
    val registry = new BindingRegistry(existed)
    items.addAll(additional())
    registry.register(items)

    registry.reconfig(reconfigs)
    registry.autowire()

    SpringBeanRegistry.register(registry.allBeans, springRegistry)
  }

  /** Register property editors to convert JUC to Scala collection types in bean definitions.
   *
   * @param factory bean factory to configure
   */
  override def postProcessBeanFactory(factory: ConfigurableListableBeanFactory): Unit = {
    factory.addPropertyEditorRegistrar(new ScalaEditorRegistrar)
  }

  /** Register last built-in beans such as EventMulticaster.
   *
   * @return iterable of additional registry items to register
   */
  private def additional(): Iterable[Binder.RegistryItem] = {
    val clazz = classOf[ContainerEventMulticaster]
    val multicaster = new Binder.Definition("EventMulticaster.default", clazz, null).on(Condition.missing(clazz))
    multicaster.property("container", this)
    multicaster.initMethod = Some("init")
    List(multicaster)
  }
}
