object Test {
  def f[T1 <: Tuple](o: Option[Tuple.Concat[T1, T1]]): Unit =
    o match { case Some(x) => }
}
