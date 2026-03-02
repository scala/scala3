import NamedTuple.{NamedTuple, AnyNamedTuple}

type And[X <: Boolean, Y <: Boolean] <: Boolean = (X, Y) match
  case (true, true) => true
  case _ => false
type AndLambda = [X <: Boolean, Y <: Boolean] =>> And[X, Y]

trait Expr2[Result, Scalar <: Boolean]:
  type StripScalar[E] = E match
    case Expr2[_, s] => s

  type AllScalar[A <: AnyNamedTuple] = Tuple.Fold[Tuple.Map[NamedTuple.DropNames[A], StripScalar], true, AndLambda] // error: cyclic


object Minimization:
  type And[X <: Boolean, Y <: Boolean] = (X, Y) match
    case (true, true) => true
    case _ => false

  type AndLambda = [X <: Boolean, Y <: Boolean] =>> And[X, Y]

  type All[A <: Tuple] = Tuple.Fold[A, true, AndLambda] // error: cyclic
