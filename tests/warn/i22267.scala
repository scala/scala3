//> using options -explain

import language.implicitConversions

case class M[T](value: T)
given [T]: Conversion[M[T], T] = _.value
class C:
  def m(n: Double): Unit = println(0->n)
object C:
  given Ops = Ops()
class Ops:
  extension (self: C) def m(n: Int): Unit = println(1->n)
  extension (self: C) def m(n: Double): Unit = println(2->n) // warn
  extension (self: C) def m(s: String): Unit = println(3->s)

@main def test() =
  val c = M(C())
  def i = 42
  def pi = 3.14
  c.value.m(i)
  c.value.m(pi)
  c.m(i) // conversion
  c.m(pi) // conversion
  c.m("hello, world") // extension
  //m(c)(pi)
  val c0 = C()
  c0.m(pi)
  c0.m("hello, world")
