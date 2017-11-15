package dotty.generic

sealed trait Prod[X]
final case class PCons[H[_], T[t] <: Prod[t], X](head: H[X], tail: T[X]) extends Prod[X]
final case class PNil[X]() extends Prod[X] // Cannot be put in `object Prod` because of #3422
