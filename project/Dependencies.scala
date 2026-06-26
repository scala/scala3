import sbt.*
import dotty.tools.sbtplugin.Versions

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

/**
 * All dependencies should be put here to ensure the same version of the dependency is used in all projects.
 * PLEASE KEEP ALPHABETIZED!
 */
object Dependencies {
  val asm = "org.scala-lang.modules" % "scala-asm" % "9.9.0-scala-1"

  val coursier = "io.get-coursier" %% "coursier" % "2.1.24"
  val coursierInterface = "io.get-coursier" % "interface" % "1.0.29-M4"

  val fansi = "com.lihaoyi" %% "fansi" % "0.5.1"

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

  val guava = "com.google.guava" % "guava" % "33.6.0-jre"

  private val jacksonVersion = "3.1.2"
  val jacksonDatabind = "tools.jackson.core" % "jackson-databind" % jacksonVersion
  val jacksonDataformatYaml = "tools.jackson.dataformat" % "jackson-dataformat-yaml" % jacksonVersion

  private val jlineVersion = "4.0.14"
  val jlineReader = "org.jline" % "jline-reader" % jlineVersion
  val jlineTerminal = "org.jline" % "jline-terminal" % jlineVersion
  val jlineTerminalJni = "org.jline" % "jline-terminal-jni" % jlineVersion

  val jsoup = "org.jsoup" % "jsoup" % "1.22.2"

  val liqp = "nl.big-o" % "liqp" % "0.9.2.3"

  val lsp4j = "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "1.0.0"

  val lz4 = "org.lz4" % "lz4-java" % "1.8.1"

  private val mtagsVersion = "1.6.7"
  val mtagsInterfaces = "org.scalameta" % "mtags-interfaces" % mtagsVersion
  val mtagsShared = "org.scalameta" % s"mtags-shared_${Versions.scala2Version}" % mtagsVersion

  val pprint = "com.lihaoyi" %% "pprint" % "0.9.3"

  val sbtCompilerInterface = "org.scala-sbt" % "compiler-interface" % "1.12.0"
  val sbtJunitInterface = "com.github.sbt" % "junit-interface" % "0.13.3"
  val sbtZincApiInfo = "org.scala-sbt" %% "zinc-apiinfo" % "1.12.0"

  val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.19.0"

  val scalaJsDomVersion = "2.8.1" // needs %%% which isn't usable within a val here
  val scalaJsEnvNodeJs = "org.scala-js" %% "scalajs-env-nodejs" % "1.6.0"
  val scalaJsIr = "org.scala-js" %% "scalajs-ir" % scalaJSVersion
  val scalaJsJavalib = "org.scala-js" % "scalajs-javalib" % scalaJSVersion
  val scalaJsJunitTestRuntime = "org.scala-js" %% "scalajs-junit-test-runtime" % scalaJSVersion
  val scalaJsLibrary = "org.scala-js" %% "scalajs-library" % scalaJSVersion
  val scalaJsLinker = "org.scala-js" %% "scalajs-linker" % scalaJSVersion

  val sourcecode = "com.lihaoyi" %% "sourcecode" % "0.4.4"

  val usingDirectives = "org.virtuslab" % "using_directives" % "1.1.4"
}
