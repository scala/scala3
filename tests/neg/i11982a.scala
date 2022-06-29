package tuplefun
object Unpair {

  def pair[A, B](using a: ValueOf[A], b: ValueOf[B]): Tuple2[A, B] =
    (a.value, b.value)

  def unpair[X <: Tuple2[?, ?]](
      using a: ValueOf[Tuple.Head[X]],
            b: ValueOf[Tuple.Head[Tuple.Tail[X]]]  // error
  ): Tuple2[Tuple.Head[X], Tuple.Head[Tuple.Tail[X]]] =  // error
    type AA = Tuple.Head[X]
    type BB = Tuple.Head[Tuple.Tail[X]]  // error
    pair[AA, BB](using a, b)
}