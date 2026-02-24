package dotty.tools.dotc
package config

import Settings.Setting.ChoiceWithHelp
import dotty.tools.backend.jvm.BackendUtils.classfileVersionMap
import dotty.tools.io.{AbstractFile, Directory, PlainDirectory, NoAbstractFile}

object ScalaSettingsProperties:

  private lazy val minTargetVersion = classfileVersionMap.keysIterator.min
  private lazy val maxTargetVersion = classfileVersionMap.keysIterator.max

  private val minReleaseVersion = 17

  def supportedTargetVersions: List[String] =
    (minTargetVersion to maxTargetVersion).toList.map(_.toString)

  def supportedReleaseVersions: List[String] =
    val jdkVersion = Runtime.version().feature()
    val maxVersion = Math.min(jdkVersion, maxTargetVersion)
    (minReleaseVersion to maxVersion).toList.map(_.toString)

  def supportedSourceVersions: List[String] =
    SourceVersion.values.diff(SourceVersion.illegalInSettings)
      .map(_.toString).toList

  def supportedLanguageFeatures: List[ChoiceWithHelp[String]] =
    Feature.values.map((n, d) => ChoiceWithHelp(n.toString, d))

  val legacyLanguageFeatures: List[String] =
    Feature.legacyFeatures

  def defaultClasspath: String = sys.env.getOrElse("CLASSPATH", ".")

  def defaultPageWidth: Int = {
    val defaultWidth = 80
    val columnsVar = System.getenv("COLUMNS")
    if columnsVar != null then columnsVar.toInt
    else if Properties.isWin then
      val ansiconVar = System.getenv("ANSICON") // eg. "142x32766 (142x26)"
      if ansiconVar != null && ansiconVar.matches("[0-9]+x.*") then
        ansiconVar.substring(0, ansiconVar.indexOf("x")).toInt
      else defaultWidth
    else defaultWidth
  }
