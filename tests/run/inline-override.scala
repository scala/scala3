import annotation.targetName

abstract class A:
  def f(x: Int) = s"dynamic $x"
  def h(x: Int): String
  @targetName("h2") def h1(x: Int): String
  inline def i(x: Int): String

class B extends A:
  inline override def f(x: Int) = g(x)
  inline def g(x: Int) = s"inline $x"
  inline def h(x: Int) = g(x)
  @targetName("h2") inline def h1(x: Int) = g(x)
  inline def i(x: Int) = g(x)

@main def Test =
  val b = B()
  println(b.f(22))
  println(b.h(22))
  println(b.h1(22))
  println(b.i(22))
  val a: A = b
  println(a.f(22))
  println(a.h(22))
  println(a.h1(22))
//  println(a.i(22))



