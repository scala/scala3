package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.core.Contexts.Context

class CompilationUniverse(val context: Context) extends scala.tasty.Universe {
  val tasty: TastyImpl.type = TastyImpl
}
