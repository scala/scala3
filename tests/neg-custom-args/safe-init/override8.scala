import scala.annotation.partial

trait Foo {
  val x = "world"
  val y = foo(5)

  @partial
  def foo(n: Int): String
}

trait Bar {
  val m = "hello"

  def foo(n: Int) =  m

  def foo(x: String) = "hello, " + x
}

class Qux extends Foo with Bar  // error: Bar.foo needs to be annotated with `@partial`

trait Yun {
  val m = "hello"

  @partial
  def foo(n: Int) =  m    // error // error
}


class Tao {
  val m = "hello"

  def msg = "can be overriden"

  def foo(n: Int) =  m + msg
}

class Zen extends Tao with Foo  // error: Tao.foo needs to be `@partial`

class Lux {
  val m = "hello"

  def msg = "can be overriden"

  @partial
  def foo(n: Int) =  m + msg         // error  // error // error
}

class Logos extends Lux with Foo