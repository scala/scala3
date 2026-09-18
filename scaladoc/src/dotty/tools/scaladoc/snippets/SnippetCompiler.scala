package dotty.tools.scaladoc
package snippets

import dotty.tools.io
import dotty.tools.io.{AbstractFile, VirtualFile}
import dotty.tools.dotc.Driver
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.config.Settings.Setting.*
import dotty.tools.dotc.reporting.{Reporter, StoreReporter}
import dotty.tools.dotc.util.{SourceFile, SourcePosition}
import dotty.tools.dotc.util.Spans.NoSpan
import java.nio.charset.StandardCharsets

class SnippetCompiler(
  val snippetCompilerSettings: Seq[SnippetCompilerSetting[?]],
  target: AbstractFile = io.virtualDirectory("(memory)")
):
  def compile(
    snippet: SnippetSource,
    wrappedSnippet: WrappedSnippet,
    arg: SnippetCompilerArg,
    sourceFile: SourceFile
  ): SnippetCompilationResult =
    val driver = new SnippetDriver(snippetCompilerSettings, target)
    val files = List(new VirtualFile("(snippet)", wrappedSnippet.snippet.getBytes(StandardCharsets.UTF_8)))
    val reporter = driver.processFiles(arg.scalacOptions, files)
    val diagnostics = reporter.pendingMessages
    val observed = SnippetExpectations.observe(diagnostics, wrappedSnippet, sourceFile)

    val (messages, succeeded) =
      if arg.verifyDiagnostics then
        val expected = SnippetExpectations.parse(snippet, sourceFile)
        val validation = SnippetExpectations.validate(expected, observed, sourceFile)
        val failCheck =
          if arg.flag == SCFlags.Fail && expected.expectedErrors == 0 && !reporter.hasErrors then
            List(SnippetCompilerMessage(None, s"No errors found when compiling snippet in $sourceFile:\n${wrappedSnippet.snippet}", MessageLevel.Error))
          else Nil
        val errors = validation ++ failCheck
        val hasMismatches = errors.exists(_.level == MessageLevel.Error)
        val msgs =
          if hasMismatches then errors
          else observed.map(_.message) ++ errors
        val ok = !hasMismatches
          && (arg.flag != SCFlags.Fail || reporter.hasErrors || expected.expectedErrors > 0)
        (msgs, ok)
      else
        val failMsg = Option.when(arg.flag == SCFlags.Fail && !reporter.hasErrors)(
          SnippetCompilerMessage(
            Some(Position(SourcePosition(sourceFile, NoSpan), wrappedSnippet.outerLineOffset)),
            s"Snippet should not compile but compiled successfully in $sourceFile:\n${wrappedSnippet.snippet}", MessageLevel.Error)
        )
        val msgs = observed.map(_.message) ++ failMsg
        val ok = (arg.flag == SCFlags.Fail) == reporter.hasErrors
        (msgs, ok)

    val t = Option.when(!reporter.hasErrors)(target)
    SnippetCompilationResult(wrappedSnippet, succeeded, t, messages)

final class SnippetDriver(snippetCompilerSettings: Seq[SnippetCompilerSetting[?]], target: AbstractFile) extends Driver:
  protected override def sourcesRequired: Boolean = false

  protected override def initCtx =
    val rootCtx = super.initCtx.fresh.addMode(Mode.ReadPositions).addMode(Mode.Interactive)
    rootCtx.setSetting(rootCtx.settings.experimental, true)
    rootCtx.setSetting(rootCtx.settings.YretainTrees, true)
    rootCtx.setSetting(rootCtx.settings.XcookComments, true)
    rootCtx.setSetting(rootCtx.settings.XreadComments, true)
    rootCtx.setSetting(rootCtx.settings.color, "never")
    rootCtx.setSetting(rootCtx.settings.XimportSuggestionTimeout, 0)
    rootCtx.setSetting(rootCtx.settings.fromTasty, false)
    for scSetting <- snippetCompilerSettings do
      rootCtx.setSetting(scSetting.setting, scSetting.value)
    rootCtx.setSetting(rootCtx.settings.outputDir, target)
    // don't print any diagnostics, keep them for later
    rootCtx.setReporter(new StoreReporter())

  def processFiles(args: Iterable[String], files: List[AbstractFile]): Reporter = {
    setup(args.toArray, initCtx.fresh) match
      case Some((_, compileCtx)) =>
        doCompile(newCompiler(using compileCtx), files)(using compileCtx)
      case None =>
        initCtx.reporter
  }