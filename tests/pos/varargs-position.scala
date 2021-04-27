object varargspos {
  def g(a: Int, x: Int*) = a + x.length
  g(1, 2, 3, 4)
  val xs = 1 :: 2 :: Nil
  val a = 8
  val b = 7
  g(5, xs*)
  g(3, Nil*)
  g(a, xs*)
  g(a, b, 2, 3)
  g(1)
}
