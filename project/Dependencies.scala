import java.io.FileInputStream;
import java.util.Properties

import sbt._

/** A dependency shared between multiple projects should be put here
 *  to ensure the same version of the dependency is used in all projects
 */
object Dependencies {
  private val jacksonVersion = "2.9.0"
  val `jackson-databind` =
    "com.fasterxml.jackson.core" % "jackson-databind" % jacksonVersion
  val `jackson-dataformat-yaml` =
    "com.fasterxml.jackson.dataformat" % "jackson-dataformat-yaml" % jacksonVersion

  private def readPropertyFile(file: String): Properties = {
    val prop = new Properties()
    val input = new FileInputStream(file)
    try {
      prop.load(input)
      prop
    }
    finally input.close
  }

  private val sbtVersion = readPropertyFile("project/build.properties").getProperty("sbt.version")
  val `compiler-interface` = "org.scala-sbt" % "compiler-interface" % sbtVersion
  val `zinc-apiinfo` = "org.scala-sbt" %% "zinc-apiinfo" % sbtVersion
}