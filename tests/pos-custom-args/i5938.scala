import compiletime.summonFrom

trait Link[T, A]

inline def link[T] =
  summonFrom {
    case _: Link[T, s] =>
      summonFrom {
        case stuff: s => stuff
      }
  }

class Foo
object Foo {
  erased implicit val barLink: Link[Foo, Bar.type]
}

implicit object Bar {
  def baz: Unit = ()
}

object Test extends App {
  val bar = link[Foo]
  bar.baz
}
