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

  val newCompilerInterface = "org.scala-sbt" % "compiler-interface" % "1.4.3"
  val oldCompilerInterface = "org.scala-sbt" % "compiler-interface" % "1.3.5"
}
