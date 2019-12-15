class A(n: Int) {
  val x = n

  def f: Int = x * x
}

class B(val a: A) {
  val b = a.f
}

class C(override val a: A) extends B(new A(10))  // ok

class M(val a: A)

class N(override val a: A) extends M(new A(10))

class X(val a: A) {
  a.f
}

class Y extends X(new A(10)) {
  override val a: A = ???        // error
}
