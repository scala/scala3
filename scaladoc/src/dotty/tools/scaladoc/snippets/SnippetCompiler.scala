package dotty.tools.scaladoc
package snippets

import dotty.tools.io.{AbstractFile, VirtualDirectory}
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveCompiler
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.config.Settings.Setting._
import dotty.tools.dotc.interfaces.SourcePosition
import dotty.tools.dotc.ast.Trees.Tree
import dotty.tools.dotc.interfaces.{SourceFile => ISourceFile}
import dotty.tools.dotc.reporting.{ Diagnostic, StoreReporter }
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.{ Compiler, Run }
import dotty.tools.io.{AbstractFile, VirtualDirectory}
import dotty.tools.repl.AbstractFileClassLoader
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.interfaces.Diagnostic._

class SnippetCompiler(
  classpath: String = System.getProperty("java.class.path"), //Probably needs to be done better
  val scalacOptions: String = "",
  target: AbstractFile = new VirtualDirectory("(memory)")
):

  private def newDriver: InteractiveDriver = {
    val defaultFlags =
      List("-color:never", "-unchecked", "-deprecation", "-Ximport-suggestion-timeout", "0")
    val options = scalacOptions.split("\\s+").toList
    val settings =
      options ::: defaultFlags ::: "-classpath" :: classpath :: Nil
    new InteractiveDriver(settings)
  }

  private val driver = newDriver

  private val scala3Compiler = new Compiler

  private def newRun(using ctx: Context): Run = scala3Compiler.newRun

  private def nullableMessage(msgOrNull: String): String =
    if (msgOrNull == null) "" else msgOrNull

  private def createReportMessage(diagnostics: Seq[Diagnostic]): Seq[SnippetCompilerMessage] = {
    val infos = diagnostics.toSeq.sortBy(_.pos.source.path)
    val errorMessages = infos.map {
      case diagnostic if diagnostic.position.isPresent =>
        val pos = diagnostic.position.get
        val msg = nullableMessage(diagnostic.message)
        val level = MessageLevel.fromOrdinal(diagnostic.level)
        SnippetCompilerMessage(pos.line, pos.column, pos.lineContent, msg, level)
      case d =>
        val level = MessageLevel.fromOrdinal(d.level)
        SnippetCompilerMessage(-1, -1, "", nullableMessage(d.message), level)
    }
    errorMessages
  }

  def compile(
    snippets: List[String]
  ): SnippetCompilationResult = {
    val context = driver.currentCtx.fresh
      .setSetting(
        driver.currentCtx.settings.outputDir,
        target
      )
      .setReporter(new StoreReporter)
    val run = newRun(using context)
    run.compileFromStrings(snippets)
    val messages = createReportMessage(context.reporter.pendingMessages(using context))
    val targetIfSuccessful = Option.when(!context.reporter.hasErrors)(target)
    SnippetCompilationResult(targetIfSuccessful, messages)
  }

  def compile(
    snippet: String
  ): SnippetCompilationResult = compile(List(snippet))


