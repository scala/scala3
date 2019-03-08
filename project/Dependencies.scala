import sbt._

/** A dependency shared between multiple projects should be put here
 *  to ensure the same version of the dependency is used in all projects
 */
object Dependencies {
  private val jacksonVersion = "2.9.8"
  val `jackson-databind` =
    "com.fasterxml.jackson.core" % "jackson-databind" % jacksonVersion
  val `jackson-dataformat-yaml` =
    "com.fasterxml.jackson.dataformat" % "jackson-dataformat-yaml" % jacksonVersion

  private val zincVersion = "1.2.5"
  val `compiler-interface` = "org.scala-sbt" % "compiler-interface" % zincVersion
  val `zinc-api-info` = "org.scala-sbt" %% "zinc-apiinfo" % zincVersion
}
