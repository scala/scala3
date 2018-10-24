import scala.annotation.cold

trait Foo {
  val x = "world"
  val y = foo(5)

  def foo(n: Int): String
}

trait Bar {
  val m = "hello"

  def foo(n: Int) =  m

  def foo(x: String) = "hello, " + x
}

class Qux extends Foo with Bar  // error: Bar.foo needs to be annotated with `@cold`

trait Yun {
  val m = "hello"

  @cold
  def foo(n: Int) =  m    // error // error
}


class Tao {
  val m = "hello"

  def msg = "can be overriden"

  @scala.annotation.init
  def foo(n: Int) =  m + msg
}

class Zen extends Tao with Foo

class Lux {
  val m = "hello"

  def msg = "can be overriden"

  def foo(n: Int) =  m + msg
}

class Logos extends Lux with Foo  // error