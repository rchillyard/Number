package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.matchers.{LogDebug, LogInfo, LogLevel, LogOff}
import com.typesafe.config.ConfigFactory

/**
  * A configuration object for expression-related settings. This object is responsible
  * for loading and providing specific configurations used to control behavior such as
  * log levels during expression matching.
  *
  * It uses the `ConfigFactory` to load configuration settings and determines the
  * appropriate log level for expression matching based on the `number.expression.match-log-level`
  * property from the configuration.
  *
  * The supported log levels are:
  * - "off": Corresponds to `LogOff`.
  * - "info": Corresponds to `LogInfo`.
  * - "debug": Corresponds to `LogDebug`.
  *
  * If the property is not specified or does not match any of the expected values, the log level defaults to `LogOff`.
  */
object ExpressionConfig {

  val matchLogLevel: LogLevel =
    config.getString("number.expression.match-log-level").toLowerCase match {
      case "off" => LogOff
      case "info" => LogInfo
      case "debug" => LogDebug
      case _ => LogOff
    }

  private lazy val config = ConfigFactory.load()
}