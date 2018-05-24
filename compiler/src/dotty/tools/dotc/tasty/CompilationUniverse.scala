package dotty.tools.dotc.tasty

import dotty.tools.dotc.core.Contexts.Context

class CompilationUniverse(val context: Context) extends scala.tasty.Universe {
  val tasty: TastyImpl.type = TastyImpl
}
