sealed trait Foo
object Foo:
  case object Baz extends Foo
case class Bar(x: Int) extends Foo
