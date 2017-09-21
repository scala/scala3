sealed trait Foo[T]
case class Bar[T](s: String)

object Test {
  def shouldError[T](foo: Foo[T]): String =
    foo match {
      case bar: Bar[T] => bar.s
    }
}