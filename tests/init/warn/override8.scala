trait Foo {
  val x = "world"
  val y = foo(5)

  def foo(n: Int): String
}

trait Bar {
  val m = "hello"  // warn

  def foo(n: Int) =  m

  def foo(x: String) = "hello, " + x
}

class Qux extends Foo with Bar
class Qux2 extends Bar with Foo


trait Yun {
  val m = "hello"

  def foo(n: Int) =  m
}


class Tao {
  private val m = "hello"

  private def msg = "can be overridden"

  def foo(n: Int) =  m + msg
}

class Zen extends Tao with Foo
