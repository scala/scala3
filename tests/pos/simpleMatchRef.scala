
class Foo {
  val X: Int = ???
  def foo(x: Any): Unit = x match {
    case X => println("a")
    case Y => println("b")
  }
}

object Y
