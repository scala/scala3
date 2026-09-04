package dotty.tools.repl.worksheet

import dotty.tools.directives.UsingDirectiveDiagnostic
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
      case Parsed(source, trees, _, directiveDiagnostics) =>
        val original = SourceFile.virtual(filename, text)
        val inputStatements =
          WorksheetSource.statements(original, trees)
        val directiveWarnings =
          WorksheetSession.directiveWarnings(text, directiveDiagnostics.toList)

        val baseSession =
          if current.canAppend(filename, text, inputStatements) then current
          else
            current.close()
            SessionState.initial(settings, screenWidth)

        val appended = inputStatements.drop(baseSession.inputStatements.length)
        val evaluation = baseSession.evaluator.evaluate(appended, baseSession.state)
        val accepted = baseSession.inputStatements ::: evaluation.accepted
        val accumulated = baseSession.diagnostics ::: evaluation.diagnostics
        current = baseSession.copy(
          filename = Some(filename),
          text =
            if evaluation.accepted.length == appended.length then text
            else accepted.lastOption.fold("")(last => text.take(last.end)),
          inputStatements = accepted,
          evaluatedStatements = baseSession.evaluatedStatements ::: evaluation.statements,
          state = evaluation.state,
          diagnostics = accumulated
        )
        WorksheetResult(
          directiveWarnings ::: accumulated ::: evaluation.failure,
          baseSession.evaluatedStatements ::: evaluation.statements
        )

      case SyntaxErrors(_, diagnostics, _) =>
        val retainsPrefix =
          !current.stale &&
            current.filename == Some(filename) &&
            text.startsWith(current.text)
        if !retainsPrefix then current = current.copy(stale = true)
        val previousDiagnostics = if retainsPrefix then current.diagnostics else Nil
        val previousStatements = if retainsPrefix then current.evaluatedStatements else Nil
        WorksheetResult(
          previousDiagnostics ::: diagnostics.map(WorksheetDiagnostic.fromCompiler),
          previousStatements
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
    val classification = ReplDirectives.classify(text)
    val ignoredDirectives =
      Option.when(classification.hasDirectives)(
        IgnoredDirectives -> classification.directiveLines.minOption
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
