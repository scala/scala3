trait A {
  def apply(x: Any): Int =  1
}

object B {
  def f(x: Any): Int = 2
  lazy val f = new A {}
  val x = f(null)  // error: ambiguous
}