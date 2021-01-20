sealed trait Foo
object Foo with
  case object Baz extends Foo
case class Bar(x: Int) extends Foo
