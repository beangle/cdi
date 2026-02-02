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

package org.beangle.cdi.spring.context

import org.beangle.cdi.CDILogger
import org.beangle.commons.event.{DefaultEventMulticaster, EventMulticaster}
import org.beangle.commons.lang.ClassLoaders
import org.beangle.commons.lang.reflect.Reflections
import org.beangle.commons.lang.time.Stopwatch
import org.springframework.beans.factory.BeanFactory
import org.springframework.beans.factory.support.{BeanDefinitionRegistryPostProcessor, DefaultListableBeanFactory}
import org.springframework.beans.factory.xml.{ResourceEntityResolver, XmlBeanDefinitionReader}
import org.springframework.beans.support.ResourceEditorRegistrar
import org.springframework.core.convert.ConversionService
import org.springframework.core.env.StandardEnvironment
import org.springframework.core.io.support.{PathMatchingResourcePatternResolver, ResourcePatternResolver}
import org.springframework.core.io.{DefaultResourceLoader, Resource}
import org.springframework.util.ClassUtils

/**
 * Simple BeanFactory loader
 */
class BeanFactoryLoader extends DefaultResourceLoader, ResourcePatternResolver, ContextLoader {
  var environment = new StandardEnvironment()
  var eventMulticaster: EventMulticaster = _
  var resourcePatternResolver: ResourcePatternResolver = new PathMatchingResourcePatternResolver
  var classLoader = ClassUtils.getDefaultClassLoader
  var context: DefaultListableBeanFactory = _

  override def load(id: String, contextClassName: String, configLocation: String): BeanFactory = {
    val watch = new Stopwatch(true)
    CDILogger.info(s"$id starting")

    context =
      if (null == contextClassName) new DefaultListableBeanFactory()
      else Reflections.newInstance(ClassLoaders.load(contextClassName)).asInstanceOf[DefaultListableBeanFactory]

    context.setAllowBeanDefinitionOverriding(false)
    context.setSerializationId(id)
    loadBeanDefinitions(environment.resolveRequiredPlaceholders(configLocation))
    refresh()
    CDILogger.info(s"$id started in $watch")
    context
  }

  protected def loadBeanDefinitions(configLocation: String): Unit = {
    val reader = new XmlBeanDefinitionReader(context)
    reader.setEnvironment(environment)
    reader.setResourceLoader(this)
    reader.setEntityResolver(new ResourceEntityResolver(this))
    reader.setValidating(false)
    reader.loadBeanDefinitions(configLocation)
  }

  protected def refresh(): Unit = {
    prepareBeanFactory()
    invokeBeanFactoryPostProcessors()
    initApplicationEventMulticaster()
    finishBeanFactoryInitialization()
    eventMulticaster.multicast(new BeanFactoryRefreshedEvent(context))
  }

  /**
   * Initialize the ApplicationEventMulticaster.
   */
  protected def initApplicationEventMulticaster(): Unit = {
    val multicasters = context.getBeansOfType(classOf[EventMulticaster])
    if (multicasters.isEmpty) {
      eventMulticaster = new DefaultEventMulticaster
    } else {
      eventMulticaster = multicasters.values.iterator().next()
    }
  }

  /**
   * Configure the factory's standard context characteristics,
   * such as the context's ClassLoader and post-processors.
   *
   * @param context the BeanFactory to configure
   */
  protected def prepareBeanFactory(): Unit = {
    context.setBeanClassLoader(classLoader)
    context.addPropertyEditorRegistrar(new ResourceEditorRegistrar(this, environment))

    context.registerResolvableDependency(classOf[BeanFactory], context)
    if (!context.containsLocalBean("environment")) {
      context.registerSingleton("environment", environment)
    }
    if (!context.containsLocalBean("systemProperties")) {
      context.registerSingleton("systemProperties", environment.getSystemProperties())
    }
    if (!context.containsLocalBean("systemEnvironment")) {
      context.registerSingleton("systemEnvironment", environment.getSystemEnvironment())
    }
  }

  /**
   * Instantiate and invoke all registered BeanFactoryPostProcessor beans,
   * respecting explicit order if given.
   * <p>Must be called before singleton instantiation.
   */
  protected def invokeBeanFactoryPostProcessors(): Unit = {
    val postProcessorNames = context.getBeanNamesForType(classOf[BeanDefinitionRegistryPostProcessor], true, false)
    postProcessorNames foreach { name =>
      val pp = context.getBean(name, classOf[BeanDefinitionRegistryPostProcessor])
      pp.postProcessBeanDefinitionRegistry(context)
      pp.postProcessBeanFactory(context)
    }
  }

  /**
   * Finish the initialization of this context's bean factory,
   * initializing all remaining singleton beans.
   */
  protected def finishBeanFactoryInitialization(): Unit = {
    val conversionServiceBeanName = "conversionService"
    if (context.containsBean(conversionServiceBeanName) &&
      context.isTypeMatch(conversionServiceBeanName, classOf[ConversionService])) {
      context.setConversionService(context.getBean(conversionServiceBeanName, classOf[ConversionService]))
    }

    context.setTempClassLoader(null)
    context.freezeConfiguration()
    context.preInstantiateSingletons()
  }

  override def getResources(locationPattern: String): Array[Resource] = {
    this.resourcePatternResolver.getResources(locationPattern)
  }

  override def close(): Unit = {

  }
}
