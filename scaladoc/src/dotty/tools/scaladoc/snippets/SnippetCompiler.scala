package dotty.tools.scaladoc
package snippets

import dotty.tools.io.{AbstractFile, VirtualDirectory}
import dotty.tools.dotc.Driver
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.MacroClassLoader
import dotty.tools.dotc.config.Settings.Setting._
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.{ Compiler, Run }
import dotty.tools.dotc.util.{SourceFile, SourcePosition}
import dotty.tools.dotc.util.Spans.NoSpan

class SnippetCompiler(
  val snippetCompilerSettings: Seq[SnippetCompilerSetting[?]],
  target: AbstractFile = new VirtualDirectory("(memory)")
):
  object SnippetDriver extends Driver:
    val currentCtx =
      val rootCtx = initCtx.fresh.addMode(Mode.ReadPositions).addMode(Mode.Interactive)
      rootCtx.setSetting(rootCtx.settings.experimental, true)
      rootCtx.setSetting(rootCtx.settings.YretainTrees, true)
      rootCtx.setSetting(rootCtx.settings.XcookComments, true)
      rootCtx.setSetting(rootCtx.settings.XreadComments, true)
      rootCtx.setSetting(rootCtx.settings.color, "never")
      rootCtx.setSetting(rootCtx.settings.XimportSuggestionTimeout, 0)

      val ctx = setup(Array(""), rootCtx) match
        case Some((_, ctx)) =>
          ctx
        case None => rootCtx
      val res = snippetCompilerSettings.foldLeft(ctx.fresh) { (ctx, setting) =>
        ctx.setSetting(setting.setting, setting.value)
      }
      res.initialize()(using res)
      MacroClassLoader.init(res)

  private val scala3Compiler = new Compiler

  private def newRun(using ctx: Context): Run = scala3Compiler.newRun

  def compile(
    snippet: SnippetSource,
    wrappedSnippet: WrappedSnippet,
    arg: SnippetCompilerArg,
    sourceFile: SourceFile
  ): SnippetCompilationResult =
    val baseContext = SnippetDriver.currentCtx.fresh
      .setSetting(
        SnippetDriver.currentCtx.settings.outputDir,
        target
      )
      .setReporter(new StoreReporter)
    val context =
      if arg.scalacOptions.isEmpty then baseContext
      else
        val args = arg.scalacOptions.toArray
        SnippetDriver.setup(args, baseContext) match
          case Some((_, ctx)) => ctx
          case None => baseContext
    val run = newRun(using context)
    run.compileFromStrings(List(wrappedSnippet.snippet))

    val diagnostics = context.reporter.pendingMessages(using context)
    val observed = SnippetExpectations.observe(diagnostics, wrappedSnippet, sourceFile)

    val (messages, succeeded) =
      if arg.verifyDiagnostics then
        val expected = SnippetExpectations.parse(snippet, sourceFile)
        val validation = SnippetExpectations.validate(expected, observed, sourceFile)
        val failCheck =
          if arg.flag == SCFlags.Fail && expected.expectedErrors == 0 && !context.reporter.hasErrors then
            List(SnippetCompilerMessage(None, s"No errors found when compiling snippet in $sourceFile:\n${wrappedSnippet.snippet}", MessageLevel.Error))
          else Nil
        val errors = validation ++ failCheck
        val hasMismatches = errors.exists(_.level == MessageLevel.Error)
        val msgs =
          if hasMismatches then errors
          else observed.map(_.message) ++ errors
        val ok = !hasMismatches
          && (arg.flag != SCFlags.Fail || context.reporter.hasErrors || expected.expectedErrors > 0)
        (msgs, ok)
      else
        val failMsg = Option.when(arg.flag == SCFlags.Fail && !context.reporter.hasErrors)(
          SnippetCompilerMessage(
            Some(Position(SourcePosition(sourceFile, NoSpan), wrappedSnippet.outerLineOffset)),
            s"Snippet should not compile but compiled successfully in $sourceFile:\n${wrappedSnippet.snippet}", MessageLevel.Error)
        )
        val msgs = observed.map(_.message) ++ failMsg
        val ok =
          if arg.flag == SCFlags.Fail then context.reporter.hasErrors
          else !context.reporter.hasErrors
        (msgs, ok)

    val t = Option.when(!context.reporter.hasErrors)(target)
    SnippetCompilationResult(wrappedSnippet, succeeded, t, messages)
