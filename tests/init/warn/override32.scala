trait Foo {
  val x = "world"
  val y = foo(5)

  def foo(n: Int): String
}

trait Bar {
  final val m: String = "hello"  // warn

  def foo(n: Int) =  m
}

class Qux extends Foo with Bar
class Qux2 extends Bar with Foo