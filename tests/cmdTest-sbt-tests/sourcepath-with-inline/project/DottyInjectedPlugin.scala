import sbt._
import Keys._

import java.util.Properties
import java.io.File

object DottyInjectedPlugin extends AutoPlugin {
  override def requires = plugins.JvmPlugin
  override def trigger = allRequirements

  private val packProperties = settingKey[Properties]("The properties of the dist/pack command")

  override val projectSettings = Seq(
    packProperties := {
      val prop = new Properties()
      IO
        .read(new File(sys.props("pack.version.file")))
        .linesIterator
        .foreach { line =>
          val Array(key, value) = line.split(":=")
          prop.setProperty(key, value)
        }
      prop
    },
    scalaVersion := packProperties.value.getProperty("version")
  )
}
