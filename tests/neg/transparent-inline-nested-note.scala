trait Id[T]:
  type Out <: Int

transparent inline def internal[T <: Int]: Id[T] =
  new Id[T]:
    type Out = T

transparent inline def id: Int =
  val one = internal[1]
  ??? : one.Out

val i: 1 = id // error
