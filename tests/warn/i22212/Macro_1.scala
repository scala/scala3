import scala.quoted._

object Macro {
  inline def makeMatch() = ${makeMatchImpl}
  def makeMatchImpl(using Quotes) = {
    '{
      (_: Foo) match
        case Bar => ()
    }
  }
}
