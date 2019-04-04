object Test {
  val unit = ()

  def f[T <: Tuple, X](xs: T, x: X) =
    xs ++ x *: unit
}
