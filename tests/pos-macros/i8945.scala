// src-2/MacroImpl.scala
trait Context {
  object universe {
    type Literal
  }
}

class MacroImpl(val c: Context) {
  import c.universe.*
  def mono: Literal = ???
}

// src-3/Macros.scala
import scala.language.experimental.macros

object Macros {

  object Bundles {
    def mono: Unit = macro MacroImpl.mono
    inline def mono: Unit = ${ Macros3.monoImpl }
  }

  object Macros3 {
    def monoImpl(using quoted.Quotes) = '{()}
  }

}