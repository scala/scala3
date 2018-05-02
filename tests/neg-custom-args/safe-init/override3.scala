import scala.annotation.partial

trait Foo {
  println("init x")
  val x = "world"
  val y = foo(5)

  @partial
  def foo(n: Int): String
}

class Bar1 extends Foo {
  val m = "hello"

  def foo(n: Int) = "world"             // ok
}

class Qux extends Bar1 {
  val u = "hello"

  override def foo(n: Int) = u + "world" // error // error
}

class Bar2 extends Foo {
  val m = "hello"

  final def foo(n: Int) = "world"
}

class Bar3 extends Foo {
  val m = "hello"

  final def foo(n: Int) = m + "world"  // error // error
}
