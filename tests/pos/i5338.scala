object Test {
  val emptyTuple = Tuple()

  def f[T <: Tuple, X](xs: T, x: X) =
    xs ++ x *: emptyTuple
}
