package dotty.tools.dotc.transform.patmat

import dotty.tools.dotc.core.Contexts.Context

/** Checks satisfiability of constraints in an extremely naive way */
object NaiveConstraintChecker {
  def hasUnsatisfiableConstraints(s: ConstrainedSpace)(implicit ctx: Context): Boolean =
    !termsPotentiallySatisfiable(s.termConstraints) ||
    !typesPotentiallySatisfiable(s.typeConstraints)

  def termsPotentiallySatisfiable(constraints: List[TermConstraint]): Boolean =
    constraints.forall {
      case Dummy | Neg(Dummy) => true
      case AlwaysSatisfied => true
      case Neg(AlwaysSatisfied) => false
    }

  @annotation.tailrec
  def typesPotentiallySatisfiable(constraints: List[TypeConstraint])(implicit ctx: Context): Boolean = {
    def comparePair(tpeq1: TypeEquality, tpeq2: TypeEquality) =
      if (tpeq1.tp1 =:= tpeq2.tp1) tpeq1.tp2 =:= tpeq2.tp2 else true

    @annotation.tailrec
    def compareOneVsList(tpeq: TypeEquality, constraints: List[TypeConstraint]): Boolean =
      constraints match {
        case Nil => true
        case (tpeq2: TypeEquality) :: rest =>
          comparePair(tpeq, tpeq2) && compareOneVsList(tpeq, rest)
      }

    constraints match {
      case Nil => true
      case (tpeq: TypeEquality) :: tail =>
        compareOneVsList(tpeq, tail) && typesPotentiallySatisfiable(tail)
    }
  }
}
