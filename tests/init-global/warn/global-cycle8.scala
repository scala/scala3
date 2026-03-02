class A {
  def foo() = println(O.n)
}

class B {
  val a = new A
}

object O { // warn
  val n: Int = 10
  println(P.m)
}

object P {
  val m = Q.bar(new B)
}

object Q {
  def bar(b: B) = b.a.foo()
}
