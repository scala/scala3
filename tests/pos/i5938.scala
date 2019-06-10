trait Link[T, A]

inline def link[T] = delegate match {
  case _: Link[T, s] =>
    delegate match {
      case stuff: s => stuff
    }
}

class Foo
object Foo {
  erased implicit val barLink: Link[Foo, Bar.type] = null
}

implicit object Bar {
  def baz: Unit = ()
}

object Test extends App {
  val bar = link[Foo]
  bar.baz
}
