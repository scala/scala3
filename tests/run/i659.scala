class Foo(val a: Int) {
  //def foo = { {case x => x + a}: PartialFunction[Int, Int]}
  class Bar { def result(x: Int) = x + a }
  def bar = new Bar
}

object Test extends dotty.runtime.LegacyApp {
  //val x = new Foo(1).foo.apply(2)
  val y = new Foo(1).bar.result(2)
  assert(y == 3, y)
  //assert(x == 3, x)
}
