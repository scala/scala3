package hello

import lib.*

case class Student(name: String)

class M(val n: Int) {
  val a = 30 * n
  def this(c: Char) = this(c.toInt)

  class B(x: Int) {
    def this(c: Char) = this(c.toInt)
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


object Test {
  def testLib: Unit = {
    val a: A = new A(30)
    val b: a.B = new a.B(24)
    a.foo(3)
    b.bar(9)
  }

  def testHello: Unit = {
    val a: M = new M(30)
    val b: a.B = new a.B(24)
    a.foo(3)
    b.bar(9)
  }
}
