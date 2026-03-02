import scala.language.experimental.erasedDefinitions

import compiletime.summonFrom

trait Link[T, A]

transparent inline def link[T] =
  summonFrom {
    case _: Link[T, s] =>
      summonFrom {
        case stuff: s => stuff
      }
  }

class Foo
object Foo {
  erased implicit val barLink: Link[Foo, Bar.type] = caps.unsafe.unsafeErasedValue
}

implicit object Bar {
  def baz: Unit = ()
}

object Test extends App {
  val bar = link[Foo]
  bar.baz
}
