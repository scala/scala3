package dotty.tools.dotc.reporting.diagnostic

sealed abstract class ErrorCategory(name: String) {
  override def toString: String = name
}
object ErrorCategory {

  val NoKind = new ErrorCategory("") {}

  val Compatibility = new ErrorCategory("Compatibility") {}
  val DefinitionNotFound = new ErrorCategory("Definition Not Found") {}
  val DuplicateSymbol = new ErrorCategory("Duplicate Symbol") {}
  val MatchCaseUnreachable = new ErrorCategory("Match case Unreachable") {}
  val MemberNotFound = new ErrorCategory("Member Not Found") {}
  val Naming = new ErrorCategory("Naming") {}
  val PatternMatchExhaustivity = new ErrorCategory("Pattern Match Exhaustivity") {}
  val Reference = new ErrorCategory("Reference") {}
  val Syntax = new ErrorCategory("Syntax") {}
  val TypeMismatch = new ErrorCategory("Type Mismatch") {}
  val UnboundIdentifier = new ErrorCategory("Unbound Identifier") {}
  val Usage = new ErrorCategory("Usage") {}
}
