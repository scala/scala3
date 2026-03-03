package test

class File

class A {
  val f: File^ = File()
  val g: File^ = File()

  class B uses_init A.this.f:
    val u = f.toString

  class C:
    val u = f.toString // error

  class D uses A.this.f uses_init A.this.f:
    val u = f.toString
    val v = g.toString // error
    def m = f.toString
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


