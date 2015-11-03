object Test {
  def f[A, B](x: A)(implicit e: <:<[A, B]): B = x
}
