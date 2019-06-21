object Test {
  def main(args: Array[String]): Unit = {
    // constant fold will fail for non-bootstrapped Dotty
    assert(!(Float.NaN > 0.0))
    assert(!(Float.NaN < 0.0))
    assert(!(Double.NaN > 0.0))
    assert(!(Double.NaN < 0.0))

    val f: Float = Float.NaN
    val d: Double = Double.NaN
    assert(!(f > 0.0f))
    assert(!(f < 0.0f))
    assert(!(d > 0.0))
    assert(!(d < 0.0))

    // loop forever before the fix
    var x = Double.NaN
    while(x < 10.0) { x = x + 1; println(x) }
    while(x > 10.0) { x = x + 1; println(x) }
    do { x = x + 1; println(x) } while(x < 10.0)
    do { x = x + 1; println(x) } while(x > 10.0)

    // tests from https://github.com/scala/scala/pull/5207
    {
      val n = Double.NaN
      def ne(x: Double, y: Double) = x != y
      val fs: List[(Double, Double) => Boolean] = List(_ < _, _ <= _, _ > _, _ >= _,  _ == _, (x, y) => !ne(x, y))
      val vs = List[Double](n, 1, -1, 0)
      for (f <- fs; v <- vs; (x, y) <- List((n, v), (v, n))) assert(!f(x, y))
    }

    {
      val n = Float.NaN
      def ne(x: Float, y: Float) = x != y
      val fs: List[(Float, Float) => Boolean] = List(_ < _, _ <= _, _ > _, _ >= _,  _ == _, (x, y) => !ne(x, y))
      val vs = List[Float](n, 1, -1, 0)
      for (f <- fs; v <- vs; (x, y) <- List((n, v), (v, n))) assert(!f(x, y))
    }

    {
      def a[T](x: T, y: T) = x == y
      def b[T](x: T, y: T) = x != y
      val n = Double.NaN
      (a(n, n) :: a(n, 0) :: a (0, n) :: !b(n, n) :: !b(n, 0) :: !b(0, n) :: Nil).foreach(b => assert(!b))
    }

    {
      def a[T](x: T, y: T) = x == y
      def b[T](x: T, y: T) = x != y
      val n = Float.NaN
      (a(n, n) :: a(n, 0) :: a (0, n) :: !b(n, n) :: !b(n, 0) :: !b(0, n) :: Nil).foreach(b => assert(!b))
    }
  }
}
