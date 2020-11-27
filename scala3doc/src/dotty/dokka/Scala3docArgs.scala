package dotty.dokka

import org.jetbrains.dokka._
import org.jetbrains.dokka.utilities._
import org.jetbrains.dokka.plugability._
import java.util.ServiceLoader
import java.io.File
import java.util.jar._
import collection.JavaConverters._
import collection.immutable.ArraySeq

import java.nio.file.Files

import dotty.tools.dotc.config.Settings._
import dotty.tools.dotc.config.CommonScalaSettings
import dotty.tools.dotc.report
import dotty.tools.dotc.core.Contexts._
import dotty.dokka.Scala3doc._

class Scala3docArgs extends SettingGroup with CommonScalaSettings:
  val unsupportedSettings = Seq(
    // Options that we like to support
    bootclasspath, extdirs, javabootclasspath, encoding, usejavacp,
    // Needed for plugin architecture
    plugin,disable,require, pluginsDir, pluginOptions,
    // we need support for sourcepath and sourceroot
    sourcepath, sourceroot
  )

  val sourceLinks: Setting[String] =
    StringSetting("-source-links", "sources", SourceLinks.usage, "")

  val syntax: Setting[String] =
    StringSetting("-comment-syntax", "syntax", "Syntax of the comment used", "")

  val revision: Setting[String] =
    StringSetting("-revision", "revision", "Revision (branch or ref) used to build project project", "")

object Scala3docArgs:
  def extract(args: List[String])(using Context) =
    val inst = new Scala3docArgs
    import inst._
    val initialSummary =
      ArgsSummary(defaultState, args, errors = Nil, warnings = Nil)
    val summary =
      processArguments(initialSummary, processAll = true, skipped = Nil)

    summary.warnings.foreach(report.warning(_))
    summary.errors.foreach(report.error(_))

    extension[T](arg: Setting[T]):
      def get = arg.valueIn(summary.sstate)
      def withDefault(default: => T) =
        if arg.get == arg.default then default else arg.get
      def nonDefault =
        if arg.get == arg.default then None else Some(arg.get)

    def parseTastyRoots(roots: String) =
      roots.split(File.pathSeparatorChar).toList.map(new File(_))

    val (existing, nonExisting) =
      summary.arguments.map(File(_)).partition(_.exists)

    if nonExisting.nonEmpty then report.warning(
      s"Scala3doc will ignore following nonexisiten paths: ${nonExisting.mkString(", ")}"
    )

    val (dirs, files) = existing.partition(_.isDirectory)
    val (validFiles, other) = files.partition(f =>
      f.getName.endsWith(".tasty") || f.getName.endsWith(".jar")
    )

    if other.nonEmpty then report.warning(
      s"Scala3doc suports only .tasty and .jar files, following files will be ignored: ${other.mkString(", ")}"
    )

    def defaultDest(): File =
      report.error("Destenation is missing, please provide '-d' parameter pointing to directory here docs should be created")
      File("output")

    val parseSyntax = syntax.nonDefault.fold(CommentSyntax.default){ str =>
      CommentSyntax.parse(str).getOrElse{
        report.error(s"unrecognized value for -syntax option: $str")
        CommentSyntax.default
      }
    }

    unsupportedSettings.filter(s => s.get != s.default).foreach { s =>
      report.warning(s"Setting ${s.name} is currently not supported.")
    }
    val destFile = outputDir.nonDefault.fold(defaultDest())(_.file)
    val printableProjectName = projectName.nonDefault.fold("")("for " + _ )
    report.inform(
      s"Generating documenation $printableProjectName in $destFile")

    Args(
      projectName.withDefault("root"),
      dirs,
      validFiles,
      classpath.get,
      destFile,
      siteRoot.nonDefault,
      projectVersion.nonDefault,
      projectLogo.nonDefault,
      parseSyntax,
      sourceLinks.nonDefault.fold(Nil)(_.split(",").toList),
      revision.nonDefault
    )