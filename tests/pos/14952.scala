//> using options -Xkind-projector:underscores

import Tuple.*

type LiftP[F[_], T] <: Tuple =
  T match {
    case _ *: _ => F[Head[T]] *: LiftP[F, Tail[T]]
    case _ => EmptyTuple
  }
