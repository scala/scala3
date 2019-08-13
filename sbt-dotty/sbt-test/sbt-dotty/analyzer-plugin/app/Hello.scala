package hello

import lib._

class M(val n: Int) {
  val a = 30 * n
  class B(x: Int) {
    val b = x * a
    def bar(i: Int) = i * x
  }

  def foo(i: Int) = i * n

  def bar = {
    class C(val s: String)
    val c = new C("hello")
    def qux = c.s
    qux
  }
}


object Hello {
  def testLib: Unit = {
    val a = new A(30)
    val b = new a.B(24)
    a.foo(3)
    b.bar(9)
  }

  def testHello: Unit = {
    val a = new M(30)
    val b = new a.B(24)
    a.foo(3)
    b.bar(9)
  }
}
