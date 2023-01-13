object A {
  val a: Int = B.foo()      // error
}

object B {
  def foo(): Int = A.a * 2
}
