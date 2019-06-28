class Foo(val a: Int) {
  def foo = { {case x => x + a}: PartialFunction[Int, Int]}
  class Bar { def result(x: Int) = x + a }
  def bar = new Bar
}

class VFoo(val a: Int) extends AnyVal {
  def foo = { {case x => x + a}: PartialFunction[Int, Int]}
}

object Test extends App {

  def Foo(a: Int) = {
    class Bar { def result(x: Int) = x + a }
    new Bar().result(2)
  }

  val x1 = new Foo(1).bar.result(2)
  assert(x1 == 3, s"x1 = $x1")
  val x2 = Foo(1)
  assert(x2 == 3, s"x2 = $x2")
  val x3 = new Foo(1).foo.apply(2)
  assert(x3 == 3, s"x3 = $x3")
  val x4 = new VFoo(1).foo.apply(2)
  assert(x4 == 3, s"x4 = $x4")
}
