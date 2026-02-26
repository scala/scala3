import sbt._

/** A dependency shared between multiple projects should be put here
 *  to ensure the same version of the dependency is used in all projects
 */
object Dependencies {
  private val jacksonVersion = "2.15.1"
  val `jackson-databind` =
    "com.fasterxml.jackson.core" % "jackson-databind" % jacksonVersion
  val `jackson-dataformat-yaml` =
    "com.fasterxml.jackson.dataformat" % "jackson-dataformat-yaml" % jacksonVersion

  // Freeze on 0.62.x as 0.64.0 requires Java 11
  private val flexmarkVersion = "0.62.2"

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

  val compilerInterface = "org.scala-sbt" % "compiler-interface" % "1.10.7"

  private val semanticdbScalaPBVersion = "0.11.20"
  val semanticdbProtobufVersion = "4.33.5"

  // ScalaPB scala-collection-compat_3 transitively, and
  // cousier_2.13 pulls scala-collection-compat_2.13.
  // In the meantime, pin _2.13 and exclude _3 until the coursier for3Use2_13 deps are removed.
  val semanticdbCollectionCompat213 =
    "org.scala-lang.modules" % "scala-collection-compat_2.13" % "2.13.0"

  private val semanticdbScalaPBRuntimeBase =
    ("com.thesamet.scalapb" %% "scalapb-runtime" % semanticdbScalaPBVersion)
      .exclude("org.scala-lang.modules", "scala-collection-compat_3")
      .exclude("com.thesamet.scalapb", "lenses_3")

  val semanticdbScalaPBRuntime = semanticdbScalaPBRuntimeBase
  val semanticdbScalaPBRuntimeProtobuf = semanticdbScalaPBRuntimeBase % "protobuf"
}
