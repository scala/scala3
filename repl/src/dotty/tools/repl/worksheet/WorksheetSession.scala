package dotty.tools.repl.worksheet

import dotty.tools.directives.UsingDirectiveDiagnostic
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.util.SourceFile
import dotty.tools.repl.ParseResult
import dotty.tools.repl.Parsed
import dotty.tools.repl.ReplDirectives
import dotty.tools.repl.State
import dotty.tools.repl.SyntaxErrors

private[worksheet] final class WorksheetSession(
    settings: Array[String],
    screenWidth: Int = 120
):
  @volatile private var current = SessionState.initial(settings, screenWidth)

  def evaluate(filename: String, text: String): WorksheetResult = synchronized:
    if !current.startup.isUsable then WorksheetResult(current.startup.diagnostics, Nil)
    else
      val evaluated = evaluateParsed(filename, text)
      WorksheetResult(
        current.startup.diagnostics ::: evaluated.diagnostics,
        evaluated.statements
      )

  private def evaluateParsed(filename: String, text: String): WorksheetResult =
    given State = current.state
    ParseResult.complete(text) match
      case Parsed(_, trees, _, directiveDiagnostics) =>
        val statements = WorksheetSource.statements(SourceFile.virtual(filename, text), trees)
        if !current.canAppend(filename, text, statements) then
          current.close()
          current = SessionState.initial(settings, screenWidth)
        evaluateStatements(
          filename,
          text,
          statements,
          parsesWhole = true,
          WorksheetSession.directiveWarnings(text, directiveDiagnostics.toList)
        )

      case SyntaxErrors(_, errors, trees) =>
        val statements = completeStatements(filename, text, trees)
        if !current.canAppend(filename, text, statements) then
          current.close()
          current = SessionState.initial(settings, screenWidth)
        evaluateStatements(
          filename,
          text,
          statements,
          parsesWhole = false,
          errors.map(WorksheetDiagnostic.fromCompiler)
        )

      case _ =>
        current = current.copy(stale = true)
        if text.isBlank then WorksheetResult(Nil, Nil)
        else
          WorksheetResult(
            List(
              WorksheetDiagnostic(
                WorksheetSession.lineRange(text, 0),
                "REPL commands are not supported in worksheets.",
                WorksheetDiagnosticSeverity.Error
              )
            ),
            Nil
          )

  private def completeStatements(
      filename: String,
      text: String,
      trees: List[untpd.Tree]
  )(using State): List[InputStatement] =
    WorksheetSource
      .statements(SourceFile.virtual(filename, text), trees)
      .takeWhile: statement =>
        ParseResult.complete(statement.source) match
          case _: Parsed => true
          case _ => false

  private def evaluateStatements(
      filename: String,
      text: String,
      statements: List[InputStatement],
      parsesWhole: Boolean,
      parseDiagnostics: List[WorksheetDiagnostic]
  ): WorksheetResult =
    val baseSession = current
    val appended = statements.drop(baseSession.inputStatements.length)
    val evaluation = baseSession.evaluator.evaluate(appended, baseSession.state)
    val accepted = baseSession.inputStatements ::: evaluation.accepted
    val accumulated = baseSession.diagnostics ::: evaluation.diagnostics
    val ranWholeText = parsesWhole && evaluation.accepted.length == appended.length
    current = baseSession.copy(
      filename = Some(filename),
      text =
        if ranWholeText then text
        else accepted.lastOption.fold("")(last => text.take(last.end)),
      inputStatements = accepted,
      evaluatedStatements = baseSession.evaluatedStatements ::: evaluation.statements,
      state = evaluation.state,
      diagnostics = accumulated
    )
    WorksheetResult(
      parseDiagnostics ::: accumulated ::: evaluation.failure,
      baseSession.evaluatedStatements ::: evaluation.statements
    )

  def cancel(): Unit = current.runner.cancel()

  def shutdown(): Unit =
    cancel()
    synchronized:
      current.close()

private[worksheet] object WorksheetSession:
  private val IgnoredDirectives = "REPL Worksheet PoC"

  private def directiveWarnings(
      text: String,
      parserDiagnostics: List[UsingDirectiveDiagnostic]
  ): List[WorksheetDiagnostic] =
    val declared = ReplDirectives.read(text)
    val ignoredDirectives =
      Option.when(declared.nonEmpty)(
        IgnoredDirectives -> declared.lines.map(_.number).minOption
      )
    val parsed = parserDiagnostics.map(diagnostic =>
      diagnostic.message -> Some(diagnostic.position.line)
    )
    (parsed ::: ignoredDirectives.toList).distinctBy(_._1).map: (message, line) =>
      WorksheetDiagnostic(
        line.fold(WorksheetPosition.none)(lineRange(text, _)),
        message,
        WorksheetDiagnosticSeverity.Warning
      )

  private[worksheet] def lineRange(text: String, line: Int): WorksheetPosition =
    text.linesIterator.drop(line).nextOption() match
      case Some(content) => WorksheetPosition(line, 0, line, content.length)
      case None => WorksheetPosition.none
