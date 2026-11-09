package dotty.tools.directives

enum DiagnosticSeverity:
  case Error, Warning

case class UsingDirectiveDiagnostic(
  message: String,
  severity: DiagnosticSeverity,
  position: Position
)
