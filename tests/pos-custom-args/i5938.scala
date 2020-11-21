import compiletime._

trait Link[T, A]

inline def link[T] =
  inline summonInline[Link[T, _]] match {
    case Some(_: Link[T, s]) => summonInline[s]
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
