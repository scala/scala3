trait Foo {
  val x = 20
  foo(5)                          // ok

  def foo(n: Int): Int
}


abstract class Bar extends Foo {
  foo(5)                          // ok
}

class Qux(x: Int) extends Bar {
  def foo(n: Int) = x + n        // ok
}

class Yun extends Bar {
  override val x: Int = 10
  def foo(n: Int) = x + n        // error // error
}
