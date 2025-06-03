abstract class A {
  val g = (n: Int) => n + y
  val x: Int = f(g)          // warn
  val y: Int = 10

  def f(m: Int => Int): Int
}

class B extends A {
  override def f(g: Int => Int): Int = g(20)
}
