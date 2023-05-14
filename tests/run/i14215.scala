def f[T <: Tuple2[Int, Int]](tup: T): T = tup

@main def Test: Unit =
  (1, 2)._1
  f((1, 2))._1

  (1 *: 2 *: EmptyTuple)._1
  f(1 *: 2 *: EmptyTuple)._1
  f[Int *: Int *: EmptyTuple](1 *: 2 *: EmptyTuple)._1

  f[Int *: Int *: EmptyTuple]((1, 2))._1
  f[Tuple2[Int, Int]](1 *: 2 *: EmptyTuple)._1
