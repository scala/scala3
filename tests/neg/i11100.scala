import scala.annotation.static

class C {
  val a: Int = 3
  class D
  object D {
    @static def foo: Int = a * a // error
  }
}

@main
def Test =
  val c = new C
  println(c.D.foo)
