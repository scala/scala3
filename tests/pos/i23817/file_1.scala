sealed trait N[T]
case class MakeTuple[T <: Tuple](v: T) extends N[T]
