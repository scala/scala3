package dotty.generic

sealed trait ProdK[X]
final case class PConsK[H[_], T[t] <: ProdK[t], X](head: H[X], tail: T[X]) extends ProdK[X]
final case class PNilK[X]() extends ProdK[X] // Cannot be put in `object ProdK` because of #3422
