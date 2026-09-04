package dotty.tools.repl.worksheet

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
    stale: Boolean = false
):
  def canAppend(
      nextFilename: String,
      nextText: String,
      nextStatements: List[InputStatement]
  ): Boolean =
    !stale &&
      (filename.isEmpty ||
      filename == Some(nextFilename) &&
      nextText.startsWith(text) &&
      inputStatements.zip(nextStatements).forall((previous, next) =>
        previous.start == next.start &&
          previous.end == next.end &&
          previous.source == next.source
      ) &&
      nextStatements.length >= inputStatements.length)

  def close(): Unit =
    runner.close()
    startup.close()

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
