package dotty.tools.dotc
package config

import java.nio.file.{Files, Paths}

import Settings._
import core.Contexts._
import Properties._

import scala.collection.JavaConverters._

abstract class CompilerCommand extends CliCommand:
  type ConcreteSettings = ScalaSettings

  final def helpMsg(using settings: ScalaSettings)(using SettingsState, Context): String =
    settings.allSettings.find(isHelping) match
      case Some(s) => s.description
      case _ =>
        if (settings.help.value) usageMessage
        else if (settings.Vhelp.value) vusageMessage
        else if (settings.Whelp.value) wusageMessage
        else if (settings.Xhelp.value) xusageMessage
        else if (settings.Yhelp.value) yusageMessage
        else if (settings.showPlugins.value) ctx.base.pluginDescriptions
        else if (settings.XshowPhases.value) phasesMessage
        else ""

  final def isHelpFlag(using settings: ScalaSettings)(using SettingsState): Boolean =
    import settings._
    val flags = Set(help, Vhelp,  Whelp, Xhelp, Yhelp, showPlugins, XshowPhases)
    flags.exists(_.value) || allSettings.exists(isHelping)
