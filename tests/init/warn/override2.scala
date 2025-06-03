trait Foo {
  val x = "world"
  foo(5)

  def bar(x: Int): Int = 20

  def foo(n: Int): String = x + n
}

class Bar extends Foo {
  val y = "hello"        // warn

  foo(5)
  bar(10)

  override def foo(n: Int): String = {
    println("in foo")
    y + x
  }
}

class Qux extends Foo {
  val y = "hello"

  foo(5)
  bar(10)
}
