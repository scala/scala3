package dotty.dokka

import java.util.ServiceLoader
import java.io.File
import java.util.jar._
import collection.JavaConverters._
import collection.immutable.ArraySeq

import java.nio.file.Files

import dotty.tools.dotc.config.Settings._
import dotty.tools.dotc.config.CommonScalaSettings
import dotty.tools.dotc.reporting.Reporter

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
    revision: Option[String] = None,
    externalMappings: List[ExternalDocLink] = Nil,
    identifiersToSkip: List[String] = Nil,
    regexesToSkip: List[String] = Nil,
    rootDocPath: Option[String] = None
  )

  def run(args: Array[String], rootContext: CompilerContext): Reporter =
    val (parsedArgs, ctx) = Scala3docArgs.extract(args.toList, rootContext)
    given CompilerContext = ctx

    def listTastyFiles(f: File): Seq[File] =
      val (files, dirs) = Option(f.listFiles()).toArray.flatten.partition(_.isFile)
      ArraySeq.unsafeWrapArray(
        files.filter(_.getName.endsWith(".tasty")) ++ dirs.flatMap(listTastyFiles)
      )
    val tastyFiles = parsedArgs.tastyFiles ++ parsedArgs.tastyDirs.flatMap(listTastyFiles)

    if !ctx.reporter.hasErrors then
      val updatedArgs = parsedArgs.copy(tastyDirs = Nil, tastyFiles = tastyFiles)

      if (parsedArgs.output.exists()) util.IO.delete(parsedArgs.output)

      run(updatedArgs)
      report.inform("Done")
    else report.error("Failure")
    ctx.reporter


  private [dokka] def run(args: Args)(using ctx: CompilerContext): DocContext =
    given docContext: DocContext = new DocContext(args, ctx)
    val module = ScalaModuleProvider.mkModule()

    new dotty.dokka.renderers.HtmlRenderer(module.rootPackage, module.members).render()
    report.inform("generation completed successfully")
    docContext

