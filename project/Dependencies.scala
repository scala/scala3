import sbt._

/** A dependency shared between multiple projects should be put here
 *  to ensure the same version of the dependency is used in all projects
 */
object Dependencies {
  private val jacksonVersion = "3.1.2"
  val `jackson-databind` =
    "tools.jackson.core" % "jackson-databind" % jacksonVersion
  val `jackson-dataformat-yaml` =
    "tools.jackson.dataformat" % "jackson-dataformat-yaml" % jacksonVersion

  private val flexmarkVersion = "0.64.8"

  val flexmarkDeps = Seq(
    "com.vladsch.flexmark" % "flexmark" % flexmarkVersion,
    "com.vladsch.flexmark" % "flexmark-util-ast" % flexmarkVersion,
    "com.vladsch.flexmark" % "flexmark-util-data" % flexmarkVersion,
    "com.vladsch.flexmark" % "flexmark-util-html" % flexmarkVersion,
    "com.vladsch.flexmark" % "flexmark-ext-anchorlink" % flexmarkVersion,
    "com.vladsch.flexmark" % "flexmark-ext-autolink" % flexmarkVersion,
    "com.vladsch.flexmark" % "flexmark-ext-emoji" % flexmarkVersion,
    "com.vladsch.flexmark" % "flexmark-ext-gfm-strikethrough" % flexmarkVersion,
    "com.vladsch.flexmark" % "flexmark-ext-gfm-tasklist" % flexmarkVersion,
    "com.vladsch.flexmark" % "flexmark-ext-wikilink" % flexmarkVersion,
    "com.vladsch.flexmark" % "flexmark-ext-tables" % flexmarkVersion,
    "com.vladsch.flexmark" % "flexmark-ext-yaml-front-matter" % flexmarkVersion,
  )

  val compilerInterface = "org.scala-sbt" % "compiler-interface" % "1.12.0"
}
