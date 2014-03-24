object overloaded {

  def f(x: String): String = x
  def f[T >: Null](x: T): Int = 1

  val x1 = f("abc")
  val x2 = f(new Integer(1))
  val x3 = f(null)

  val x4: String => String = f
  val x5: String => Any = f
  val x6: Any = f _

  def g(): Int = 1
  def g(x: Int): Int = 2

  val y1: Int => Int = g
  val y2: Any = g _

  println(g)

  val xs = List("a", "b")
  xs.mkString
}
