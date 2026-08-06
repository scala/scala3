object types {
  opaque type TupleGiven[T <: Tuple] <: T = T

  object TupleGiven {
    given emptyTuple: TupleGiven[EmptyTuple] = EmptyTuple
    given tupleN[H, T <: Tuple](using h: H, t: TupleGiven[T]): TupleGiven[H *: T] = h *: t
  }
}

given i: Int = 1
given s: String = "s"

val t = summon[types.TupleGiven[(Int, String)]]

@main def Test =
  assert(t._1 == 1)
  assert(t._2 == "s")
  assert((t: (Int, String))._1 == 1)
