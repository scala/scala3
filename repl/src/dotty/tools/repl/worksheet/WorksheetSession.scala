package dotty.tools.repl.worksheet

import dotty.tools.directives.UsingDirectiveDiagnostic
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.util.NoSourcePosition
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.repl.ParseResult
import dotty.tools.repl.Parsed
import dotty.tools.repl.ReplDirectives
import dotty.tools.repl.ReplDirectives.DirectiveLines
import dotty.tools.repl.State
import dotty.tools.repl.SyntaxErrors

private[worksheet] final class WorksheetSession(
    settings: Array[String],
    screenWidth: Int = 120
):
  @volatile private var current = SessionState.initial(settings, screenWidth)

  @volatile private var evaluating: Option[Thread] = None

  def evaluate(filename: String, text: String): WorksheetResult = synchronized:
    if !current.startup.isUsable then WorksheetResult(current.startup.diagnostics, Nil)
    else
      current.runner.beginEvaluation()
      evaluating = Some(Thread.currentThread)
      val evaluated =
        try evaluateParsed(filename, text)
        finally
          evaluating = None
          if current.runner.isCancelled then Thread.interrupted()

      evaluated.copy(
        diagnostics =
          current.startup.diagnostics ::: evaluated.diagnostics ::: cancellation(evaluated, SourceFile.virtual(filename, text))
      )

  private def cancellation(evaluated: WorksheetResult, source: SourceFile): List[WorksheetDiagnostic] =
    if !current.runner.isCancelled then Nil
    else if evaluated.diagnostics.exists(_.message == WorksheetDiagnostic.cancelled) then Nil
    else
      List(
        WorksheetDiagnostic(
          WorksheetSession.lineRange(source, 0),
          WorksheetDiagnostic.cancelled,
          WorksheetDiagnosticSeverity.Error
        )
      )

  private def evaluateParsed(filename: String, text: String): WorksheetResult =
    given State = current.state
    val source = SourceFile.virtual(filename, text)
    val declared = ReplDirectives.read(text)
    ParseResult(source) match
      case Parsed(_, trees, _, directiveDiagnostics) =>
        val statements = WorksheetSource.statements(source, trees)
        if !current.canAppend(filename, text, statements, declared.lines) then
          current.close()
          current = SessionState.initial(settings, screenWidth)
        evaluateStatements(
          filename,
          source,
          declared,
          statements,
          parsesWhole = true,
          WorksheetSession.directiveWarnings(source, directiveDiagnostics.toList)
        )

      case SyntaxErrors(_, errors, trees) =>
        val statements = completeStatements(source, trees)
        if !current.canAppend(filename, text, statements, declared.lines) then
          current.close()
          current = SessionState.initial(settings, screenWidth)
        evaluateStatements(
          filename,
          source,
          declared,
          statements,
          parsesWhole = false,
          errors.map(WorksheetDiagnostic.fromCompiler)
        )

      case _ => // the rest of possible values related to commands, which are not supported
        current = current.copy(stale = true)
        if text.isBlank then WorksheetResult(Nil, Nil)
        else
          WorksheetResult(
            List(
              WorksheetDiagnostic(
                WorksheetSession.lineRange(source, 0),
                "REPL commands are not supported in worksheets.",
                WorksheetDiagnosticSeverity.Error
              )
            ),
            Nil
          )

  private def completeStatements(
      source: SourceFile,
      trees: List[untpd.Tree]
  )(using State): List[InputStatement] =
    WorksheetSource
      .statements(source, trees)
      .takeWhile: statement =>
        ParseResult.complete(statement.source) match
          case _: Parsed => true
          case _ => false

  private def evaluateStatements(
      filename: String,
      source: SourceFile,
      declared: DirectiveLines,
      statements: List[InputStatement],
      parsesWhole: Boolean,
      parseDiagnostics: List[WorksheetDiagnostic]
  ): WorksheetResult =
    val baseSession =
      if current.filename.isDefined then current
      else
        val outcome = WorksheetDependencies.resolve(declared, source, current.state)
        current.runner.addToClasspath(outcome.classpath, outcome.state)
        current.copy(
          state = outcome.state,
          diagnostics = outcome.diagnostics,
          dependencies = outcome.dependencies,
          repositories = outcome.repositories,
          extraClasspath = outcome.classpath.map(_.toPath),
          directiveLines = declared.lines
        )

    current = baseSession

    val appended = statements.drop(baseSession.inputStatements.length)
    val evaluation = baseSession.compiler.evaluate(appended, baseSession.state)
    val accepted = baseSession.inputStatements ::: evaluation.accepted
    val accumulated = baseSession.diagnostics ::: evaluation.diagnostics
    val ranWholeText = parsesWhole && evaluation.accepted.length == appended.length
    current = baseSession.copy(
      filename = Some(filename),
      text =
        if ranWholeText then source.textContent()
        else accepted.lastOption.fold("")(last => source.textContent().take(last.end)),
      inputStatements = accepted,
      evaluatedStatements = baseSession.evaluatedStatements ::: evaluation.statements,
      state = evaluation.state,
      diagnostics = accumulated
    )
    WorksheetResult(
      parseDiagnostics ::: accumulated ::: evaluation.failure,
      baseSession.evaluatedStatements ::: evaluation.statements,
      baseSession.dependencies,
      baseSession.repositories,
      baseSession.extraClasspath
    )

  def cancel(): Unit =
    current.runner.cancel()
    evaluating.foreach(_.interrupt())

  def shutdown(): Unit =
    cancel()
    synchronized:
      current.close()

private[worksheet] object WorksheetSession:
  private def directiveWarnings(
      source: SourceFile,
      parserDiagnostics: List[UsingDirectiveDiagnostic]
  ): List[WorksheetDiagnostic] =
    parserDiagnostics
      .distinctBy(diagnostic => (diagnostic.message, diagnostic.position.line))
      .map: diagnostic =>
        WorksheetDiagnostic(
          lineRange(source, diagnostic.position.line),
          diagnostic.message,
          WorksheetDiagnosticSeverity.Warning
        )

  private[worksheet] def lineRange(source: SourceFile, line: Int): SourcePosition =
    source.textContent().linesIterator.drop(line).nextOption() match
      case Some(content) =>
        val start = source.lineToOffset(line)
        source.atSpan(Span(start, start + content.length))
      case None => NoSourcePosition
