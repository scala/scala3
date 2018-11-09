trait Foo {
  val x = "world"
  val y = foo(5)

  def foo(n: Int): String
}

trait Bar {
  final val m = "hello"

  @scala.annotation.init
  def foo(n: Int) =  m
}

class Qux extends Foo with Bar  // error: Bar.foo needs to be annotated with `@icy`
