class A {
  def p(cb: Int => Int): Int = cb(0)

  val q: List[Int] = {
    def f(x: Int): Int => Int = y => p(f(y))
    List(1, 2).map(f(3))
  }
  val n: Int = 4
}