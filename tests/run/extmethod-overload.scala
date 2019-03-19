object Test extends App {
  // warmup
  def f(x: Int)(y: Int) = y
  def f(x: Int)(y: String) = y.length
  assert(f(1)(2) == 2)
  assert(f(1)("two") == 3)

  def g[T](x: T)(y: Int) = y
  def g[T](x: T)(y: String) = y.length
  assert(g[Int](1)(2) == 2)
  assert(g[Int](1)("two") == 3)
  assert(g(1)(2) == 2)
  assert(g(1)("two") == 3)

  def h[T](x: T)(y: T)(z: Int) = z
  def h[T](x: T)(y: T)(z: String) = z.length
  assert(h[Int](1)(1)(2) == 2)
  assert(h[Int](1)(1)("two") == 3)
  assert(h(1)(1)(2) == 2)
  assert(h(1)(1)("two") == 3)

  implied Foo {
    def (x: Int) |+| (y: Int) = x + y
    def (x: Int) |+| (y: String) = x + y.length

    def (xs: List[T]) +++ [T] (ys: List[T]): List[T] = xs ++ ys ++ ys
    def (xs: List[T]) +++ [T] (ys: Iterator[T]): List[T] = xs ++ ys ++ ys
  }

  assert((1 |+| 2) == 3)
  assert((1 |+| "2") == 2)

  val xs = List(1, 2)
  assert((xs +++ xs).length == 6)
  assert((xs +++ xs.iterator).length == 4, xs +++ xs.iterator)
}