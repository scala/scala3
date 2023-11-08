object A {
  val a: Int = B.foo()
}

object B {
  def foo(): Int = A.a * 2  
}

// nopos-error: No warnings can be incurred under -Werror.