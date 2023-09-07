class A {
  def foo(a: Int) = {
    lazy val x: Int = if (a == 0) x else 0
    println(x)
  }
  foo(0)

  val y = 5
}
