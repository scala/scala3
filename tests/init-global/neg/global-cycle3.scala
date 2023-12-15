class A(x: Int) {
  def foo(): Int = B.a + 10  
}

object B {
  val a: Int = A(4).foo()
}

// nopos-error: No warnings can be incurred under -Werror.