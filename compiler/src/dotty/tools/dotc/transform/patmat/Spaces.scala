package dotty.tools.dotc.transform.patmat

import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.Type

/** Space definition */
sealed trait Space

/** Space representing the set of all values of a type
 *
 * @param tp: the type this space represents
 * @param decomposed: does the space result from decomposition? Used for pretty print
 *
 */
case class Typ(tp: Type, decomposed: Boolean = false) extends Space

/** Space representing an extractor pattern */
case class Prod(tp: Type, unappTp: Type, unappSym: Symbol, params: List[Space], full: Boolean) extends Space

/** Represents a set of _vectors_ of values, possibly constrained
  *
  * Before and after exhaustivity checks, [[vec]] is exactly one space long.
  */
case class ConstrainedSpace(
    vec: List[Space],
    termConstraints: List[TermConstraint],
    typeConstraints: List[TypeConstraint]
) {
  def withPrependendSpace(s: Space) = copy(vec = s :: vec)
  def withVec(vec: List[Space]) = copy(vec = vec)
  def withTermConstraints(termConstraints: List[TermConstraint]) = copy(termConstraints = termConstraints)
  def withTypeConstraints(typeConstraints: List[TypeConstraint]) = copy(typeConstraints = typeConstraints)
}

/** A term-level constraint */
sealed trait TermConstraint {
  final def neg: TermConstraint = this match {
    case Neg(c) => c
    case c: PositiveTermConstraint => Neg(c)
  }
}

/** Negated constraint. Cannot be nested by construction */
case class Neg(c: PositiveTermConstraint) extends TermConstraint

/** A constraint which isn't negated */
sealed trait PositiveTermConstraint extends TermConstraint

/** Represents a presence of some constraint (like a pattern guard) */
case object Dummy extends PositiveTermConstraint

/** A constraint which is always satisfied */
case object AlwaysSatisfied extends PositiveTermConstraint

/** A type-level constraint */
sealed trait TypeConstraint
case class TypeEquality(tp1: Type, tp2: Type) extends TypeConstraint
