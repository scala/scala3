// SIP-80 v1 limitation: a leading-dot expression cannot appear directly as the
// RHS of an infix operator. The Scala parser's `canStartInfixExprTokens` set
// does not include `.`, so `c == .Red` is parsed as the val def `c`, then a
// stray `==`.
//
// Workarounds for CUSTOM infix methods (verified by hand):
//   c matchesColor (.Red)   -- parens make `(` the starter token
//   c.matchesColor(.Red)    -- method syntax
//   c |+| (.Red)            -- operator-style with parens
//
// For BUILT-IN equality/eq/ne the RHS expected type is `Any`, so even
// `c == (.Red)` cannot resolve `Red` (Any has no useful companion). The
// idiomatic alternative is `c == Color.Red` or pattern matching.
import scala.language.experimental.targetTypedCompanionShorthand

sealed trait Color
object Color:
  case object Red extends Color

object Test:
  val c: Color = Color.Red

  // Built-in `==`: even if parsed, RHS expected type is `Any` and `.Red` would
  // not resolve. Documented as a parse error here.
  val q1: Boolean = c == .Red // error // error

  // Custom infix likewise fails to parse.
  extension (lhs: Color) infix def matchesColor(rhs: Color): Boolean = lhs == rhs
  val q2: Boolean = c matchesColor .Red // error // error

  // Operator-style custom method: same issue.
  extension (lhs: Color) def |+|(rhs: Color): Color = rhs
  val q3: Color = c |+| .Red // error

  // Workarounds (each compiles fine — uncomment to verify):
  // val ok1: Color   = c |+| (.Red)
  // val ok2: Boolean = c.matchesColor(.Red)

  // Union types as the expected type also have no companion to resolve `.X`.
  val u: Color | Int = .Red // error
