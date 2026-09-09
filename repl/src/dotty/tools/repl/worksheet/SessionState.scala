package dotty.tools.repl.worksheet

import dotty.tools.repl.ReplDirectives.DirectiveLine
import dotty.tools.repl.State

private final case class SessionState(
    startup: ReplStartup,
    runner: StatementRunner,
    evaluator: StatementEvaluator,
    filename: Option[String],
    text: String,
    inputStatements: List[InputStatement],
    evaluatedStatements: List[WorksheetStatement],
    state: State,
    diagnostics: List[WorksheetDiagnostic],
    dependencies: List[WorksheetDependency] = Nil,
    repositories: List[String] = Nil,
    extraClasspath: List[java.nio.file.Path] = Nil,
    directiveLines: List[DirectiveLine] = Nil,
    stale: Boolean = false
):
  def canAppend(
      nextFilename: String,
      nextText: String,
      nextStatements: List[InputStatement],
      nextDirectiveLines: List[DirectiveLine]
  ): Boolean =
    !stale &&
      (filename.isEmpty ||
      filename == Some(nextFilename) &&
      directiveLines == nextDirectiveLines &&
      nextText.startsWith(text) &&
      inputStatements.zip(nextStatements).forall((previous, next) =>
        previous.start == next.start &&
          previous.end == next.end &&
          previous.source == next.source
      ) &&
      nextStatements.length >= inputStatements.length)

  def close(): Unit = startup.close()

private object SessionState:
  def initial(settings: Array[String], screenWidth: Int): SessionState =
    val startup = new ReplStartup(settings)
    val runner = new StatementRunner(startup, screenWidth)
    SessionState(
      startup,
      runner,
      new StatementEvaluator(startup, runner),
      None,
      "",
      Nil,
      Nil,
      startup.initialState,
      Nil
    )
