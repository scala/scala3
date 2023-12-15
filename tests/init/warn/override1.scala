trait Foo {
  val x = 20
  foo(5)

  def foo(n: Int): Int
}


abstract class Bar extends Foo {
  foo(5)
}

class Qux(x: Int) extends Bar {
  def foo(n: Int) = x + n        // ok
}

class Yun extends Bar {
  override val x: Int = 10       // warn
  def foo(n: Int) = x + n
}
