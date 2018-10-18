trait Foo {
  val x = "world"
  foo(5)

  def bar(x: Int): Int

  private def foo(n: Int): String = x + n
}

abstract class Bar extends Foo {
  val y = "hello"

  foo(5)
  bar(10) // error

  def foo(n: Int): String = {  // need to be private or final
    println("in foo")
    println(y.size)
    y + x
  }
}
