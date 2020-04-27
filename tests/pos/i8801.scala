object Foo:
 locally {
    case class Baz()
  }

class Foo2 {
  def foo(x: Any = 3, y: Any = 9): Any = x
  val a = foo(y = { case class Bar(x: Int) }, x = ())
}