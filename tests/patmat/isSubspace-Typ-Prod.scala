case class Foo[T](x: T)
class Bar extends Foo[String]("")

def test(x: Any) = x match
  case Foo(1) =>
  case _: Bar => // used to warn about unreachable case
  // case _: Foo[_] => // still warns, something else is wrong
