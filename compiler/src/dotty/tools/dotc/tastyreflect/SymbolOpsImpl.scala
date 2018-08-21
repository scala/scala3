package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Symbols._

trait SymbolOpsImpl extends scala.tasty.reflect.SymbolOps with TastyCoreImpl {

  def SymbolasDeco(symbol: Symbol): SymbolasAPI = new SymbolasAPI {
    def isEmpty: Boolean = symbol eq NoSymbol
    def localContext(implicit ctx: Context): Context = ctx.withOwner(symbol)
    def tree(implicit ctx: Context): Option[Definition] =
      if (isEmpty) None else Some(FromSymbol.definitionFromSym(symbol))
  }

}
