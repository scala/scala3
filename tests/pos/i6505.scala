class Foo {

  type E[X]

  def i: Int = ???
  def e: E[Int] = ???

  // Transforms `(T1, ... Tn)` into `(E[T1], ..., E[Tn])`
  type F[T <: Tuple] <: Tuple = T match {
    case EmptyTuple => EmptyTuple
    case h *: t => E[h] *: F[t]
  }

  def foo1[Args <: Tuple](args: Args, args2: F[Args]): Unit = ()

  foo1((i, i), (e, e)) // fails
  foo1((i, i), (e, e): F[(Int, Int)]) // fails

}

class Foo2 {

  type E[X]

  def i: Int = ???
  def e: E[Int] = ???

  // Transforms `(T1, ... Tn)` into `(E[T1], ..., E[Tn])`
  type F[T <: Tuple] <: Tuple = T match {
    case EmptyTuple => EmptyTuple
    case h *: t => E[h] *: F[t]
  }

  def foo2[Args <: Tuple, Args2 >: F[Args] <: F[Args]](args: Args, args2: Args2): Unit = ()

  foo2((i, i), (e, e)) // fails

  // all these work
  foo2[(Int, Int), F[(Int, Int)]]((i, i), (e, e))
  foo2[(Int, Int), F[(Int, Int)]]((i, i), (e, e))
  foo2[(Int, Int), F[Int *: Int *: EmptyTuple]]((i, i), (e, e))
  foo2[(Int, Int), (E[Int], E[Int])]((i, i), (e, e))
  foo2[(Int, Int), E[Int] *: E[Int] *: EmptyTuple]((i, i), (e, e))

}