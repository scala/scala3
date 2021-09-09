package dotty.tools.scaladoc
package snippets

import collection.JavaConverters._
import scala.util.chaining._

import dotty.tools.dotc.config.Settings._
import dotty.tools.dotc.config.AllScalaSettings

import com.virtuslab.using_directives._
import com.virtuslab.using_directives.custom.model._

object UsingDirectivesSettingsExtractor:
  // Probably in the future we need to support more settings
  def process(uds: Map[Path, Value[_]]): Seq[SnippetCompilerSetting[_]] = uds.flatMap {
    case (path, value) if path.getPath.asScala.toList == List("compiler", "setting") => Seq(value)
    case (path, value) => Nil
  }.flatMap(processValue).toList.pipe { settings =>
    val allScalaSettings = new SettingGroup with AllScalaSettings
    val argsSummary = allScalaSettings.processArguments(settings, true)
    val settingsState = argsSummary.sstate
    val nonDefaultSettings = allScalaSettings.allSettings
      .filter(s => !s.isDefaultIn(settingsState))

    // TODO: Report warnings and errors in args summary
    nonDefaultSettings.map(s => SnippetCompilerSetting(s, s.valueIn(settingsState)))
  }

  def processValue(v: Value[_]): List[String] = v match {
    case v: ListValue => v.get().asScala.toList.flatMap(processValue)
    case v => List(v.toString)
  }
