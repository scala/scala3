package dotty.generic

sealed trait Prod
final case class PCons[H, T <: Prod](head: H, tail: T) extends Prod
final case class PNil() extends Prod // TODO: could be a case object....
