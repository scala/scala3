package dotty.dokka

import org.jetbrains.dokka._
import org.jetbrains.dokka.utilities._
import org.jetbrains.dokka.plugability._
import java.util.ServiceLoader
import java.io.File
import java.util.jar._
import collection.JavaConverters._
import collection.immutable.ArraySeq

import scala.tasty.inspector.TastyInspector
import java.nio.file.Files

import dotty.tools.dotc.config.Settings._
import dotty.tools.dotc.config.CommonScalaSettings
import dotty.tools.dotc.report
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.reporting.Reporter

class Scala3DocDokkaLogger(using Context) extends DokkaLogger:
  def debug(msg: String): Unit = report.debuglog(msg)

  // We do not want errors from dokka (that are) not critical to fail our runs
  def error(msg: String): Unit =
    errors += 1
    report.warning(msg)

  def info(msg: String): Unit = report.inform(msg)
  def progress(msg: String): Unit = report.informProgress(msg)
  def warn(msg: String): Unit =
    warnings += 1
    report.warning(msg)

  private var errors = 0
  private var warnings = 0
  def getErrorsCount(): Int = errors
  def getWarningsCount(): Int = warnings
  def setErrorsCount(count: Int): Unit = errors = count
  def setWarningsCount(count: Int): Unit = warnings = count

object Scala3doc:
  enum CommentSyntax:
    case Wiki
    case Markdown

  object CommentSyntax:
    def parse(str: String) = str match
        case "wiki" => Some(CommentSyntax.Wiki)
        case "markdown" => Some(CommentSyntax.Markdown)
        case _ => None

    val default = CommentSyntax.Markdown

  case class Args(
    name: String,
    tastyDirs: Seq[File] = Nil,
    tastyFiles: Seq[File] = Nil,
    classpath: String = "",
    output: File,
    docsRoot: Option[String] = None,
    projectVersion: Option[String] = None,
    projectLogo: Option[String] = None,
    defaultSyntax: CommentSyntax = CommentSyntax.Markdown,
    sourceLinks: List[String] = Nil,
    revision: Option[String] = None
  )

  def run(args: Array[String])(using Context): Reporter =
    val parsedArgs = Scala3docArgs.extract(args.toList)

    def listTastyFiles(f: File): Seq[File] =
      val (files, dirs) = Option(f.listFiles()).toArray.flatten.partition(_.isFile)
      ArraySeq.unsafeWrapArray(
        files.filter(_.getName.endsWith(".tasty")) ++ dirs.flatMap(listTastyFiles)
      )
    val tastyFiles = parsedArgs.tastyFiles ++ parsedArgs.tastyDirs.flatMap(listTastyFiles)

    val reporter = summon[Context].reporter
    if !reporter.hasErrors then
      val updatedArgs = parsedArgs.copy(tastyDirs = Nil, tastyFiles = tastyFiles)

      if (parsedArgs.output.exists()) IO.delete(parsedArgs.output)

      run(updatedArgs, new Scala3DocDokkaLogger)
      report.inform("Done")
    else report.error("Failure")
    reporter

  private [dokka] def run(args: Args, logger: DokkaLogger = DokkaConsoleLogger.INSTANCE) =
    new DokkaGenerator(new DottyDokkaConfig(args), logger).generate()


