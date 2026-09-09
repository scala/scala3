package dotty.tools.repl.worksheet

import dotty.tools.dotc.interfaces
import dotty.tools.dotc.reporting.Diagnostic
import dotty.tools.dotc.util.{SourceFile, SourcePosition}
import dotty.tools.dotc.util.Spans.Span

private[worksheet] final case class WorksheetResult(
    diagnostics: List[WorksheetDiagnostic],
    statements: List[WorksheetStatement],
    dependencies: List[WorksheetDependency] = Nil,
    repositories: List[String] = Nil,
    classpath: List[java.nio.file.Path] = Nil
)

private[worksheet] final case class WorksheetDependency(
    organization: String,
    moduleName: String,
    version: String
)

private[worksheet] final case class WorksheetStatement(
    position: WorksheetPosition,
    summary: String,
    details: String,
    isSummaryComplete: Boolean
)

private[worksheet] final case class WorksheetDiagnostic(
    position: WorksheetPosition,
    message: String,
    severity: WorksheetDiagnosticSeverity
)

private[worksheet] object WorksheetDiagnostic:
  val cancelled = "The worksheet evaluation was cancelled."

  def fromCompiler(diagnostic: Diagnostic): WorksheetDiagnostic =
    fromCompiler(diagnostic, WorksheetPosition.fromCompiler(diagnostic.pos))

  def fromCompiler(
      diagnostic: Diagnostic,
      position: WorksheetPosition
  ): WorksheetDiagnostic =
    WorksheetDiagnostic(
      position,
      diagnostic.msg.message,
      WorksheetDiagnosticSeverity.fromCompiler(diagnostic.level)
    )

private[worksheet] enum WorksheetDiagnosticSeverity:
  case Info, Warning, Error

private[worksheet] object WorksheetDiagnosticSeverity:
  def fromCompiler(level: Int): WorksheetDiagnosticSeverity =
    level match
      case interfaces.Diagnostic.ERROR => WorksheetDiagnosticSeverity.Error
      case interfaces.Diagnostic.WARNING => WorksheetDiagnosticSeverity.Warning
      case _ => WorksheetDiagnosticSeverity.Info

private[worksheet] final case class WorksheetPosition(
    startLine: Int,
    startColumn: Int,
    endLine: Int,
    endColumn: Int
)

private[worksheet] object WorksheetPosition:
  val none: WorksheetPosition = WorksheetPosition(-1, -1, -1, -1)

  def fromCompiler(position: SourcePosition): WorksheetPosition =
    if position.exists then
      WorksheetPosition(
        position.startLine,
        position.startColumn,
        position.endLine,
        position.endColumn
      )
    else none

  def fromOffsets(source: SourceFile, start: Int, end: Int): WorksheetPosition =
    fromCompiler(source.atSpan(Span(start, end)))

/** Compiler options as the worksheet needs to read and rewrite them. */
private[worksheet] object WorksheetOptions:
  private val classpathOptions = Set("-classpath", "-cp", "--class-path")

  def withoutClasspath(options: List[String]): (List[String], List[String]) =
    def loop(
        remaining: List[String],
        entries: List[String],
        kept: List[String]
    ): (List[String], List[String]) =
      remaining match
        case option :: value :: tail if classpathOptions.contains(option) =>
          loop(tail, entries :+ value, kept)
        case option :: tail if classpathOptions.exists(name => option.startsWith(s"$name:")) =>
          loop(tail, entries :+ option.substring(option.indexOf(':') + 1), kept)
        case option :: tail => loop(tail, entries, kept :+ option)
        case Nil => (entries, kept)
    loop(options, Nil, Nil)
