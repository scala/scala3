trait Foo {
  //println("hello")
  def meth(x: Int): Int
}

trait Bar(x: Int)

class C extends Foo() with Bar(1) {
  def meth(x: Int) = x
}

object Test extends C with App
