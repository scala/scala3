trait A {
  def apply(x: Any): Int =  1
}

object B {
  def f(x: Any): Int = 2
  lazy val f = new A {}
  val x = f(null)  // error: ambiguous
}

object Test {
  def f(x: Any) = 10
  val f: Foo123 = f(1)  // error: Not found: type Foo123

}
