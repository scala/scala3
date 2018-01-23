sealed trait Foo
case class Bar(s: String)

object Test {
  def shouldError(foo: Foo): String =
    foo match {
      case bar: Bar => bar.s
    }
}