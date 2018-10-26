class A(n: Int) {
  val x = n

  def f: Int = x * x
}

class B(val a: A) {
  val b = a.f
}

class C(override val a: Cold[A]) extends B(new A(10))  // error

class M(val a: Cold[A])

class N(override val a: Cold[A]) extends M(new A(10))

class X(val a: Cold[A])

class Y(override val a: A) extends X(new A(10))
