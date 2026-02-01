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

import org.beangle.cdi.config.{BindingLoader, BindingRegistry}
import org.beangle.cdi.spring.context.ContainerEventMulticaster
import org.beangle.commons.cdi.{Binder, Condition}
import org.beangle.commons.lang.time.Stopwatch
import org.beangle.commons.logging.Logging
import org.springframework.beans.factory.support.*
import org.springframework.core.io.Resource
import org.springframework.util.ClassUtils

/** BindModuleProcessor完成bean的自动注册和再配置
 * 他是BeanFactoryPostProcessor的子接口，会执行的更早一些
 *
 * @author chaostone
 */
abstract class BindModuleProcessor extends BeanDefinitionRegistryPostProcessor, Logging {

  var moduleLocations: Array[Resource] = Array.empty

  private val classLoader = ClassUtils.getDefaultClassLoader

  /** Automate register and wire bean
   * Reconfig beans
   */
  override def postProcessBeanDefinitionRegistry(springRegistry: BeanDefinitionRegistry): Unit = {
    //load bind and reconfig module,
    val watch = Stopwatch.start()
    val (modules, reconfigs) = BindingLoader.loadModules("beangle.xml")
    val items = BindingLoader.loadRegistryItems(modules)
    val existed = SpringBeanRegistry.findBeans(springRegistry)
    logger.info(s"Load ${items.size} beans in $watch")

    //reconfig autowire and register
    val registry = new BindingRegistry(existed)
    items.addAll(additional())
    registry.register(items)

    registry.reconfig(reconfigs)
    registry.autowire()

    // register to spring container
    SpringBeanRegistry.register(registry.allBeans, springRegistry)
  }

  /** register last buildin beans. */
  private def additional(): Iterable[Binder.RegistryItem] = {
    val clazz = classOf[ContainerEventMulticaster]
    val multicaster = new Binder.Definition("EventMulticaster.default", clazz, null).on(Condition.missing(clazz))
    multicaster.property("container", this)
    List(multicaster)
  }
}
