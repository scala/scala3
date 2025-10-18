class A {
  val field_a = 5
  def bar(): Int = A.this.field_a
}

class B extends A {
  val field_b = field_a
  class C {
    def bar2() = B.this.field_b
    val field_c = bar() // expands to B.this.bar()
    val field_c2 = field_a // C --> outer B --> parent A
  }
}

object O:
  val b = new B
  class D extends b.C { // D --> parent C --> outer B
    val field_d = bar2()
  }
  val d = new D
