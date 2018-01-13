trait T {
  case object Foo
}

object Bar extends T
object Baz extends T

object Test {
  def main(args: Array[String]): Unit = {
    assert(Bar.Foo eq Bar.Foo)
    assert(Bar.Foo ne Baz.Foo)
  }
}
