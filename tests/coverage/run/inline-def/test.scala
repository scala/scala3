abstract class B:
  val x: Int
  def foo: String

class A extends B:
  inline val x = 1
  inline val y = 2
  inline def foo: String = "foo".toString
  inline def bar: String = "bar".toString

@main
def Test: Unit =
  val a = A()
  println(a.x)
  println(a.foo)
  println(a.bar)
  val b: B = a
  println(b.foo)
