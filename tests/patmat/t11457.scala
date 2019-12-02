sealed abstract class Foo(val a: String)

object Foo {
  def unapply(foo: Foo): Some[String] =
    Some(foo.a)
}

class Issue11457 {
  val root: PartialFunction[Foo, Boolean] = {
    case Foo("a") => true
    case Foo("b") => false
  }
}
