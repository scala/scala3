package dotty.tools.repl.worksheet

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Phases.typerPhase
import dotty.tools.dotc.core.Phases.unfusedPhases
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.StdNames.str
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.repl.CollectTopLevelImports
import dotty.tools.repl.ParseResult
import dotty.tools.repl.Parsed
import dotty.tools.repl.ReplCompiler
import dotty.tools.repl.State
import dotty.tools.repl.SyntaxErrors
import dotty.tools.dotc.reporting.Diagnostic

private final case class WorksheetEvaluation(
    statements: List[WorksheetStatement],
    accepted: List[InputStatement],
    state: State,
    diagnostics: List[WorksheetDiagnostic],
    failure: List[WorksheetDiagnostic]
)

private final case class CompiledStatement(
    input: InputStatement,
    objectIndex: Int,
    state: State
)

private final class StatementEvaluator(startup: ReplStartup, runner: StatementRunner):
  private val compiler = new ReplCompiler

  def evaluate(
      statements: List[InputStatement],
      state: State
  ): WorksheetEvaluation =
    if statements.nonEmpty then runner.beginRun(state)

    @annotation.tailrec
    def loop(
        remaining: List[InputStatement],
        currentState: State,
        accepted: List[InputStatement],
        rendered: List[WorksheetStatement],
        diagnostics: List[WorksheetDiagnostic]
    ): WorksheetEvaluation =
      def stop(nextState: State, failure: List[WorksheetDiagnostic]) =
        WorksheetEvaluation(
          rendered.reverse,
          accepted.reverse,
          nextState,
          diagnostics,
          failure
        )

      remaining match
        case Nil =>
          WorksheetEvaluation(rendered.reverse, accepted.reverse, currentState, diagnostics, Nil)
        case statement :: tail =>
          compileOne(statement, currentState) match
            case Left((failedState, failedDiagnostics)) =>
              val nextState =
                if failedState.objectIndex == currentState.objectIndex then failedState
                else
                  failedState.copy(
                    invalidObjectIndexes =
                      failedState.invalidObjectIndexes + failedState.objectIndex
                  )
              stop(nextState, failedDiagnostics)
            case Right((compiled, warnings)) =>
              val outcome = runner.runOne(compiled, compiled.state)
              val next = outcome.rendered.fold(rendered)(_ :: rendered)
              outcome.failure match
                case Some(failure) =>
                  WorksheetEvaluation(
                    next.reverse,
                    accepted.reverse,
                    outcome.state,
                    diagnostics ::: warnings,
                    failure :: Nil
                  )
                case None =>
                  loop(tail, outcome.state, statement :: accepted, next, diagnostics ::: warnings)

    loop(statements, state, Nil, Nil, Nil)

  private def compileOne(
      statement: InputStatement,
      state: State
  ): Either[
    (State, List[WorksheetDiagnostic]),
    (CompiledStatement, List[WorksheetDiagnostic])
  ] =
    def diagnostic(compilerDiagnostic: Diagnostic) =
      WorksheetDiagnostic.fromCompiler(
        compilerDiagnostic,
        statement.mapPosition(compilerDiagnostic.pos)
      )

    given State = state
    ParseResult.complete(statement.source) match
      case parsed: Parsed =>
        startup.driver.propagateLanguageImports(parsed.trees)
        val run =
          compiler.newRun(startup.driver.replRootContext.fresh.setReporter(parsed.reporter), state)
        given compileState: State = state.copy(
          context = run.runContext.withSource(parsed.source)
        )
        compiler.compile(parsed, printSummary = false) match
          case Left((errors, errorState)) =>
            Left(errorState -> errors.map(diagnostic))
          case Right((unit, nextState)) =>
            val imports = StatementEvaluator.extractTopLevelImports(nextState.context)
            val stateWithImports = nextState.copy(
              imports =
                if imports.isEmpty then nextState.imports
                else nextState.imports.updated(nextState.objectIndex, imports),
              context = StatementEvaluator.contextWithNewImports(nextState.context, imports)
            )
            val warnings = nextState.context.reporter
              .removeBufferedMessages(using nextState.context)
              .map(diagnostic)
            val reclaimed =
              StatementEvaluator.reclaimableResults(nextState.objectIndex, nextState.valIndex)(using
                nextState.context
              )
            Right(
              CompiledStatement(
                statement,
                nextState.objectIndex,
                stateWithImports.copy(valIndex = stateWithImports.valIndex - reclaimed)
              ) -> warnings
            )
      case SyntaxErrors(_, diagnostics, _) =>
        Left(state -> diagnostics.map(diagnostic))
      case _ =>
        Left(
          state -> List(
            WorksheetDiagnostic(
              statement.position,
              "Unable to parse the worksheet statement.",
              WorksheetDiagnosticSeverity.Error
            )
          )
        )

private object StatementEvaluator:
  private def extractTopLevelImports(context: Context): List[tpd.Import] =
    unfusedPhases(using context)
      .collectFirst { case phase: CollectTopLevelImports => phase.imports }
      .getOrElse(Nil)

  private def contextWithNewImports(
      context: Context,
      imports: List[tpd.Import]
  ): Context =
    if imports.isEmpty then context
    else
      imports.foldLeft(context.fresh.setNewScope)((current, imported) =>
        current.importContext(imported, imported.symbol(using current))
      )

  private def reclaimableResults(objectIndex: Int, valIndex: Int)(using Context): Int =
    atPhase(typerPhase.next):
      val path = nme.EMPTY_PACKAGE ++ "." ++ ReplCompiler.objectNames(objectIndex)
      ReplCompiler.reclaimableResults(requiredModule(path).info.fields, valIndex)
