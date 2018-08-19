object Test {

  def f(x: Int): Int = x
  def f(x: String): String = x
  def f(x: Any): Any = x

  rewrite def g(x: Any) = f(x)
  rewrite def h(x: Any) = this.f(x)

  locally {
    val x1 = g(1)
    val x2 = g("bb")
    val y1: Int = x1
    val y2: String = x2
  }
  locally {
    val x1 = h(1)
    val x2 = h("bb")
    val y1: Int = x1
    val y2: String = x2
  }
}