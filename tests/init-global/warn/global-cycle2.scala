object A {
  val a: Int = B.foo()
}

object B {
  def foo(): Int = A.a * 2 // warn
}
