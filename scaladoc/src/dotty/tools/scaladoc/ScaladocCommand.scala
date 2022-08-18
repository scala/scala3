package dotty.tools.scaladoc

import java.util.ServiceLoader
import java.io.File
import java.util.jar._
import scala.jdk.CollectionConverters._
import collection.immutable.ArraySeq

import java.nio.file.Files

import dotty.tools.dotc.config.Settings._
import dotty.tools.dotc.config.CommonScalaSettings
import dotty.tools.scaladoc.Scaladoc._
import dotty.tools.dotc.config.Settings.Setting.value
import dotty.tools.dotc.config.Properties._
import dotty.tools.dotc.config.CliCommand
import dotty.tools.dotc.core.Contexts._

object ScaladocCommand extends CliCommand:
  type ConcreteSettings = ScaladocSettings
  override def cmdName: String = "scaladoc"
  override def versionMsg: String = s"Scaladoc $versionString -- $copyrightString"
  override def ifErrorsMsg: String = "  scaladoc -help  gives more information"

  def helpMsg(using settings: ScaladocSettings)(using SettingsState, Context): String =
    if (settings.help.value) usageMessage
    else ""

  def isHelpFlag(using settings: ScaladocSettings)(using SettingsState): Boolean =
    Set(settings.help) exists (_.value)
