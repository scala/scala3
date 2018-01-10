package dotty.tools.dotc.reporting.diagnostic

sealed case class ErrorCategory(name: String) {
  override def toString: String = name
}
object ErrorCategories {
  val COMPATIBILITY = ErrorCategory("Compatibility")
  val DEFINITION_NOT_FOUND = ErrorCategory("Definition Not Found")
  val DUPLICATE_SYMBOL = ErrorCategory("Duplicate Symbol")
  val MATCH_CASE_UNREACHABLE = ErrorCategory("Match case Unreachable")
  val MEMBER_NOT_FOUND = ErrorCategory("Member Not Found")
  val NAMING = ErrorCategory("Naming")
  val NO_KIND = ErrorCategory("")
  val PATTERN_MATCH_EXHAUSTIVITY = ErrorCategory("Pattern Match Exhaustivity")
  val REFERENCE = ErrorCategory("Reference")
  val SYNTAX = ErrorCategory("Syntax")
  val TYPE_MISMATCH = ErrorCategory("Type Mismatch")
  val UNBOUND_IDENTIFIER = ErrorCategory("Unbound Identifier")
  val USAGE = ErrorCategory("Usage")
}
