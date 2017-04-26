import sbt._
import Keys._

object DottyInjectedPlugin extends AutoPlugin {
  override def requires = plugins.JvmPlugin
  override def trigger = allRequirements

  override val projectSettings = Seq(
    scalaVersion := "0.1.1-bin-SNAPSHOT",
    scalaOrganization := "ch.epfl.lamp",
    scalacOptions += "-language:Scala2",
    scalaBinaryVersion  := "0.1"
  )
}
