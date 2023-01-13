class A(x: Int) {
  def foo(): Int = B.a + 10
}

object B {                    // error
  val a: Int = A(4).foo()
}
