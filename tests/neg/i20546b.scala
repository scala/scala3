object Minimization:
  type And[X <: Boolean, Y <: Boolean] = (X, Y) match
    case (true, true) => true
    case _ => false

  type AndLambda = [X <: Boolean, Y <: Boolean] =>> And[X, Y]

  type All[A <: Tuple] = Tuple.Fold[A, true, AndLambda] // error: cyclic
