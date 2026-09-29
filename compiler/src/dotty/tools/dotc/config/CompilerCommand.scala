package dotty.tools.dotc
package config

import Settings.*
import core.Contexts.*

abstract class CompilerCommand extends CliCommand:
  type ConcreteSettings = ScalaSettings

  final def helpMsg(using settings: ConcreteSettings)(using SettingsState, Context): String =
    settings.allSettings.find(isHelping) match
      case Some(s) => availableOptionsMsg(_ == s, shortDescription = false, showArgFileMsg = false)
      case _ =>
        if (settings.help.value) usageMessage
        else if (settings.Vhelp.value) vusageMessage
        else if (settings.Whelp.value) wusageMessage
        else if (settings.Xhelp.value) xusageMessage
        else if (settings.Yhelp.value) yusageMessage
        else if (settings.showPlugins.value) ctx.base.pluginDescriptions
        else if (settings.XshowPhases.value) phasesMessage
        else ""

  final def isHelpFlag(using settings: ConcreteSettings)(using SettingsState): Boolean =
    import settings.*
    val flags = Set(help, Vhelp,  Whelp, Xhelp, Yhelp, showPlugins, XshowPhases)
    // Check `isHelping` first so that `:help` is detected before we call `_.value` where it would fail if the setting is not a Boolean
    allSettings.exists(isHelping) || flags.exists(_.value)
