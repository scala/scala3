package dotty.tools.dotc
package config

import java.nio.file.{Files, Paths}

import Settings._
import core.Contexts._
import Properties._

import scala.collection.JavaConverters._

object CompilerCommand extends CliCommand:
  type ConcreteSettings = ScalaSettings
  override def cmdName: String = "scalac"
  override def versionMsg: String = s"Scala compiler $versionString -- $copyrightString"
  override def ifErrorsMsg: String = "  scalac -help  gives more information"

  def infoMessage(using settings: ScalaSettings)(using SettingsState)(using Context): String =
    if (settings.help.value) usageMessage
    else if (settings.Xhelp.value) xusageMessage
    else if (settings.Yhelp.value) yusageMessage
    else if (settings.showPlugins.value) ctx.base.pluginDescriptions
    else if (settings.XshowPhases.value) phasesMessage
    else ""

  def shouldStopWithInfo(using settings: ScalaSettings)(using SettingsState): Boolean =
    Set(settings.help, settings.Xhelp, settings.Yhelp, settings.showPlugins, settings.XshowPhases) exists (_.value)
