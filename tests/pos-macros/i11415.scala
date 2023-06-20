package example

import scala.language.experimental.macros

trait Context {

  val universe: Universe

  trait Universe {
    type Tree >: Null <: AnyRef with TreeApi
    type Literal >: Null <: LiteralApi with TermTree
    type TermTree >: Null <: TermTreeApi with Tree

    trait TermTreeApi extends TreeApi { this: TermTree => }
    trait LiteralApi extends TermTreeApi { this: Literal => }
    trait TreeApi extends Product { this: Tree => }
  }
}

object MacroCompat {

  object Bundles {
    def mono: Int = macro Macros2.MacroImpl.mono
    inline def mono: Int = ${ Macros3.monoImpl }
  }

  object Macros2 {
    class MacroImpl(val c: Context) {
      import c.universe._

      def mono: Literal = ???
    }
  }

  object Macros3 {
    import quoted._

    def monoImpl(using Quotes) = '{1}

  }

}
