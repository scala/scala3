package dotty.generic

sealed trait SumK[X]
sealed trait SConsK[H[_], T[t] <: SumK[t], X] extends SumK[X]
final case class SLeftK[H[_], T[t] <: SumK[t], X](head: H[X]) extends SConsK[H, T, X]
final case class SRightK[H[_], T[t] <: SumK[t], X](tail: T[X]) extends SConsK[H, T, X]
sealed trait SNilK[X] extends SumK[X]
