class A(x: Int) {
  def foo(): Int = B.a + 10 // warn
}

object B {
  val a: Int = A(4).foo()
}
