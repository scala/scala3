class A {
  class B
}

class C(val a: A, val b: a.B)

@main def Test =
  val a = A()
  val b = a.B()
  val c = C(a, b)
  val d = c.b
  val d1: c.a.B = d
