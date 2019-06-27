package dotty.tools
package dotc
package reporting
package diagnostic

enum ErrorCategory(val kind: String) {
  case NoKind extends ErrorCategory("")
  case Compatibility extends ErrorCategory("Compatibility")
  case Cyclic extends ErrorCategory("Cyclic")
  case DefinitionNotFound extends ErrorCategory("Definition Not Found")
  case DuplicateSymbol extends ErrorCategory("Duplicate Symbol")
  case MatchCaseUnreachable extends ErrorCategory("Match Case Unreachable")
  case MemberNotFound extends ErrorCategory("Member Not Found")
  case Naming extends ErrorCategory("Naming")
  case OnlyNullMatched extends ErrorCategory("Only null Matched")
  case Overload extends ErrorCategory("Overload")
  case PatternMatchExhaustivity extends ErrorCategory("Pattern Match Exhaustivity")
  case PotentialIssue extends ErrorCategory("Potential Issue")
  case Reference extends ErrorCategory("Reference")
  case Syntax extends ErrorCategory("Syntax")
  case TypeMismatch extends ErrorCategory("Type Mismatch")
  case UnboundIdentifier extends ErrorCategory("Unbound Identifier")
  case Usage extends ErrorCategory("Usage")
}
