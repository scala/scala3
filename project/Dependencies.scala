import sbt._

/** A dependency shared between multiple projects should be put here
 *  to ensure the same version of the dependency is used in all projects
 */
object Dependencies {
  private val jacksonVersion = "2.12.1"
  val `jackson-databind` =
    "com.fasterxml.jackson.core" % "jackson-databind" % jacksonVersion
  val `jackson-dataformat-yaml` =
    "com.fasterxml.jackson.dataformat" % "jackson-dataformat-yaml" % jacksonVersion

  private val flexmarkVersion = "0.42.12"

  val flexmarkDeps = Seq(
    "com.vladsch.flexmark" % "flexmark" % flexmarkVersion,
    "com.vladsch.flexmark" % "flexmark-html-parser" % flexmarkVersion,
    "com.vladsch.flexmark" % "flexmark-ext-anchorlink" % flexmarkVersion,
    "com.vladsch.flexmark" % "flexmark-ext-autolink" % flexmarkVersion,
    "com.vladsch.flexmark" % "flexmark-ext-emoji" % flexmarkVersion,
    "com.vladsch.flexmark" % "flexmark-ext-gfm-strikethrough" % flexmarkVersion,
    "com.vladsch.flexmark" % "flexmark-ext-gfm-tables" % flexmarkVersion,
    "com.vladsch.flexmark" % "flexmark-ext-gfm-tasklist" % flexmarkVersion,
    "com.vladsch.flexmark" % "flexmark-ext-wikilink" % flexmarkVersion,
    "com.vladsch.flexmark" % "flexmark-ext-yaml-front-matter" % flexmarkVersion,
  )

  val newCompilerInterface = "org.scala-sbt" % "compiler-interface" % "1.7.1"
  val oldCompilerInterface = "org.scala-sbt" % "compiler-interface" % "1.3.5"
}
