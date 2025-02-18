import scala.quoted._

sealed trait Foo
case object Bar extends Foo
case object Baz extends Foo

object Macro {
  inline def makeMatch() = ${makeMatchImpl}
  def makeMatchImpl(using Quotes) = {
    '{
      (_: Foo) match
        case Bar => ()
    }
  }
}
