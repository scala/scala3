class File

class A {
  val f: File^ = File()

  class B():
    val u = f.toString
}

def test(c: File^) =
  val a = A()
  val b = a.B()
  val _: a.B = b

  val f = () => a.B()
  val _: () ->{a.f} a.B = f
  val _: () -> a.B = f // error

  class C:
    val x = c.toString

  val g = () => C()
  val _: () ->{c} C = g
  val _: () -> C = g // error


