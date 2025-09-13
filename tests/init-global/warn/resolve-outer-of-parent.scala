class A {
  val field_a = 5
  def bar(): Int = A.this.field_a
}

class B extends A {
  def foo() = O.d // warn
  class C {
    def bar2() = B.this.foo()
    val field_c = bar() // expands to B.this.bar()
  }
}

object O:
  val b = new B
  class D extends b.C { // D --> parent C --> outer B
    val field_d = bar2()
  }
  val d = new D
