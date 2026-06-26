package dotty.tools.dotc
package config

import Settings.Setting.ChoiceWithHelp

object ScalaSettingsProperties:

  private val minTargetVersion = 17
  private val maxTargetVersion = 26
  private val minReleaseVersion = 17

  def supportedTargetVersions: Vector[String] =
    (minTargetVersion to maxTargetVersion).toVector.map(_.toString)

  def supportedReleaseVersions: Vector[String] =
    val jdkVersion = Runtime.version().feature()
    val maxVersion = Math.min(jdkVersion, maxTargetVersion)
    (minReleaseVersion to maxVersion).toVector.map(_.toString)

  def supportedSourceVersions: Vector[String] =
    SourceVersion.values.diff(SourceVersion.illegalInSettings)
      .map(_.toString).toVector

  def supportedLanguageFeatures: Vector[ChoiceWithHelp[String]] =
    Feature.values.map((n, d) => ChoiceWithHelp(n.toString, d))

  val legacyLanguageFeatures: Vector[String] =
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
