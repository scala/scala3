object Test {
  def call(k: (Int, Int) => Unit): Unit = ???
  def test = call({ case (x, y) => ()})

  trait X extends Function1[Int, String]
  implicit def f2x(f: Function1[Int, String]): X = ???
  ({case _ if "".isEmpty => ""} : X) // allowed, implicit view used to adapt

  // ({case _ if "".isEmpty => 0} : X) // expected String, found Int

  def unary[T](a: T, b: T, f: ((T, T)) => T): T = f((a, b))
  unary(1, 2, (x, y) => x)
  unary(1, 2, (x: Int, y) => x)
  unary(1, 2, (x: Int, y: Int) => x)

  val xs = List(1, 2, 3)
  def f(x: Int, y: Int) = x * y
  xs.zipWithIndex.map(_ + _)
  xs.zipWithIndex.map(f)
}
