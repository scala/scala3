trait Foo {
  val x = "world"
  foo(5)

  def bar(x: Int): Int = 20

  def foo(n: Int): String = x + n
}

class Bar extends Foo {
  val y = "hello"

  foo(5)
  bar(10) // error

  override def foo(n: Int): String = {   // error
    println("in foo")
    y + x                // error // error
  }
}

class Qux extends Foo {
  val y = "hello"

  foo(5)
  bar(10) // error
}
