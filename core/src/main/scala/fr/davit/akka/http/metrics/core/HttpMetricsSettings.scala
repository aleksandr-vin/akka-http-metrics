/*
 * Copyright 2019 Michel Davit
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fr.davit.akka.http.metrics.core

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import com.typesafe.config.{Config, ConfigException}
import fr.davit.akka.http.metrics.core.HttpMetricsRegistry.RawDimension
import fr.davit.akka.http.metrics.core.HttpMetricsSettings.HttpMetricsSettingsImpl

import scala.collection.immutable
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex

trait HttpMetricsSettings {

  /**
    * Metrics namespace
    */
  def namespace: String

  /**
    * Name of the individual metrics
    */
  def metricsNames: HttpMetricsNames

  /**
    * Function that defines if the http response should be
    * counted as an error
    */
  def defineError: HttpResponse => Boolean

  /**
    * Include the method dimension on metrics
    */
  def includeMethodDimension: Boolean

  /**
    * Include the path dimension on metrics
    */
  def includePathDimension: Boolean

  /**
    * Include the status group dimension on metrics
    */
  def includeStatusDimension: Boolean

  /**
    * Include custom labels, using the Dimension object
    */
  def serverDimensions: immutable.Seq[Dimension]

  def withNamespace(namespace: String): HttpMetricsSettings
  def withMetricsNames(metricsNames: HttpMetricsNames): HttpMetricsSettings
  def withDefineError(fn: HttpResponse => Boolean): HttpMetricsSettings
  def withIncludeMethodDimension(include: Boolean): HttpMetricsSettings
  def withIncludePathDimension(include: Boolean): HttpMetricsSettings
  def withIncludeStatusDimension(include: Boolean): HttpMetricsSettings
  def withServerDimensions(labels: immutable.Seq[Dimension]): HttpMetricsSettings
}

object HttpMetricsSettings {

  val ConfigPrefix           = "akka.http.server.metrics"
  val ServerDimension: Regex = """(.+)=(.+)""".r

  def defaultError(response: HttpResponse): Boolean = response.status match {
    case _: StatusCodes.ServerError => true
    case _                          => false
  }

  private[metrics] case class HttpMetricsSettingsImpl(
      namespace: String,
      metricsNames: HttpMetricsNames,
      defineError: HttpResponse => Boolean,
      includeMethodDimension: Boolean,
      includePathDimension: Boolean,
      includeStatusDimension: Boolean,
      serverDimensions: immutable.Seq[Dimension] = immutable.Seq.empty[Dimension]
  ) extends HttpMetricsSettings {

    def withNamespace(namespace: String): HttpMetricsSettings                       = copy(namespace = namespace)
    def withMetricsNames(metricsNames: HttpMetricsNames): HttpMetricsSettings       = copy(metricsNames = metricsNames)
    def withDefineError(fn: HttpResponse => Boolean): HttpMetricsSettings           = copy(defineError = fn)
    def withIncludeMethodDimension(include: Boolean): HttpMetricsSettings           = copy(includeMethodDimension = include)
    def withIncludePathDimension(include: Boolean): HttpMetricsSettings             = copy(includePathDimension = include)
    def withIncludeStatusDimension(include: Boolean): HttpMetricsSettings           = copy(includeStatusDimension = include)
    def withServerDimensions(labels: immutable.Seq[Dimension]): HttpMetricsSettings = copy(serverDimensions = labels)
  }

  def apply(
      namespace: String,
      metricsNames: HttpMetricsNames,
      defineError: HttpResponse => Boolean,
      includeMethodDimension: Boolean,
      includePathDimension: Boolean,
      includeStatusDimension: Boolean,
      serverDimensions: immutable.Seq[Dimension]
  ): HttpMetricsSettings = HttpMetricsSettingsImpl(
    namespace,
    metricsNames,
    defineError,
    includeMethodDimension,
    includePathDimension,
    includeStatusDimension,
    serverDimensions
  )

}

trait HttpMetricsSettingsCompanion[T <: HttpMetricsSettings] {

  def apply(system: ActorSystem): HttpMetricsSettings =
    apply(system.settings.config.getConfig(HttpMetricsSettings.ConfigPrefix))

  def apply(config: Config): HttpMetricsSettings = {
    HttpMetricsSettingsImpl(
      config.getString("namespace"),
      HttpMetricsNames(config.getConfig("names")),
      HttpMetricsSettings.defaultError,
      config.getBoolean("include-method-dimension"),
      config.getBoolean("include-path-dimension"),
      config.getBoolean("include-status-dimension"),
      config
        .getStringList("server-dimensions")
        .asScala
        .map {
          case HttpMetricsSettings.ServerDimension(key, value) =>
            RawDimension(key, value)
          case faulty =>
            throw new ConfigException.BadValue(
              config.origin(),
              "server-dimensions",
              s"expected 'key=value', got $faulty"
            )
        }
        .toList
    )
  }

}
