trait A {
  def foo(): Int
}

class B extends A {
  def foo(): Int = 10
}

class C extends A {
  def foo(): Int = O.a + 10       
}

class D(x: Int) {
  def bar(): A = if x > 0 then new B else new C
}

object O {
  val a: Int = D(5).bar().foo()
}

// nopos-error: No warnings can be incurred under -Werror.