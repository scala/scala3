object Test {

  class C {
    type T
    val f: T = ???
    def foo(x: T): T = x
  }

  var x = new C
  val y = x.foo(???)

}
object Test1 {

  class Box[B](x: B)

  class C {
    type T
    val box: Box[T] = ???
    def f(x: T): T = ???
    def x: T = ???
  }

  def c: C = new C

  val b = c.box

  val f = c.f _

}


