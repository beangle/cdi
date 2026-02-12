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
import org.beangle.cdi.config.{ContainerHooks, ContainerLoader}
import org.beangle.commons.cdi.Container
import org.beangle.commons.lang.time.Stopwatch
import org.beangle.commons.lang.{Objects, Strings}
import org.springframework.beans.factory.BeanFactory
import org.springframework.beans.factory.support.{BeanDefinitionRegistryPostProcessor, DefaultListableBeanFactory}
import org.springframework.beans.support.ResourceEditorRegistrar
import org.springframework.core.convert.ConversionService
import org.springframework.core.env.StandardEnvironment
import org.springframework.core.io.support.{PathMatchingResourcePatternResolver, ResourcePatternResolver}
import org.springframework.core.io.{DefaultResourceLoader, Resource}
import org.springframework.util.ClassUtils

/** Simple BeanFactory loader for Spring-based containers.
 *
 * Loads bean definitions, invokes post-processors, and notifies lifecycle listeners.
 */
class SpringContainerLoader extends DefaultResourceLoader, ResourcePatternResolver, ContainerLoader {
  private val environment = new StandardEnvironment()
  private val resourcePatternResolver = new PathMatchingResourcePatternResolver
  private val classLoader = ClassUtils.getDefaultClassLoader
  private var factory: DefaultListableBeanFactory = _

  /** Load and initialize the container.
   *
   * @param id             container identifier
   * @param configLocation path to configuration (e.g. classpath*:beangle.xml)
   * @return initialized container
   */
  override def load(id: String, configLocation: String): Container = {
    require(Strings.isNotBlank(id), "Container needs a non empty id")
    val watch = new Stopwatch(true)
    Logger.info(s"$id starting")
    factory = DefaultListableBeanFactory()
    factory.setAllowBeanDefinitionOverriding(false)
    factory.setSerializationId(id)
    val container = prepareContainer(configLocation)
    refresh(container)
    Container.register(container)
    Logger.info(s"$id started in $watch")
    container
  }

  protected def refresh(container: Container): Unit = {
    invokeBeanFactoryPostProcessors()
    finishBeanFactoryInitialization()
    ContainerHooks.notify(container)
  }

  /** Configure the factory's standard context characteristics and register built-ins.
   *
   * @param configLocation path to configuration, or blank for default
   * @return prepared Spring container
   */
  protected def prepareContainer(configLocation: String): Container = {
    factory.setBeanClassLoader(classLoader)
    factory.addPropertyEditorRegistrar(new ResourceEditorRegistrar(this, environment))
    factory.registerResolvableDependency(classOf[BeanFactory], factory)
    val container =
      if (Strings.isNotBlank(configLocation)) new SpringContainer(factory, configLocation)
      else new SpringContainer(factory)
    factory.registerSingleton("BeanContainer", container)
    container
  }

  /** Instantiate and invoke all registered BeanFactoryPostProcessor beans.
   *
   * Respects explicit order if given. Must be called before singleton instantiation.
   */
  protected def invokeBeanFactoryPostProcessors(): Unit = {
    val postProcessorNames = factory.getBeanNamesForType(classOf[BeanDefinitionRegistryPostProcessor], true, false)
    postProcessorNames foreach { name =>
      val pp = factory.getBean(name, classOf[BeanDefinitionRegistryPostProcessor])
      pp.postProcessBeanDefinitionRegistry(factory)
      pp.postProcessBeanFactory(factory)
    }
  }

  /** Finish the initialization of this context's bean factory.
   *
   * Initializes all remaining singleton beans.
   */
  protected def finishBeanFactoryInitialization(): Unit = {
    val conversionServiceBeanName = "conversionService"
    if (factory.containsBean(conversionServiceBeanName) &&
      factory.isTypeMatch(conversionServiceBeanName, classOf[ConversionService])) {
      factory.setConversionService(factory.getBean(conversionServiceBeanName, classOf[ConversionService]))
    }
    factory.setTempClassLoader(null)
    factory.freezeConfiguration()
    factory.preInstantiateSingletons()
  }

  /** Resolve resources by location pattern (e.g. classpath*:config/xxx.xml).
   *
   * @param locationPattern Ant-style resource location pattern
   * @return array of resolved resources
   */
  override def getResources(locationPattern: String): Array[Resource] = {
    this.resourcePatternResolver.getResources(locationPattern)
  }
}

object SpringContainerLoader {

  /** Load container with default id "ROOT" and no config location. */
  def load(): Container = {
    new SpringContainerLoader().load("ROOT", null)
  }

  /** Load container with given id and config location.
   *
   * @param id             container id, defaults to "ROOT" if null
   * @param configLocation config path, may be null
   * @return initialized container
   */
  def load(id: String, configLocation: String): Container = {
    new SpringContainerLoader().load(Objects.nvl(id, "ROOT"), null)
  }
}
