sealed trait TermConstraint {
  final def neg: TermConstraint = this match {
    case Neg(c) => c
    case c: PositiveTermConstraint => Neg(c)
  }
}
case class Neg(c: PositiveTermConstraint) extends TermConstraint
sealed trait PositiveTermConstraint extends TermConstraint

case object Dummy extends PositiveTermConstraint
