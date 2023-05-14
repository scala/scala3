trait A {
  val s = "same val in A"
  def f: String = s
}

class B extends A {
  class C {
    def call_f_in_A: String = B.super[A].f
  }
}
val b = new B
val c = new b.C
@main def Test = c.call_f_in_A // AbstractMethodError