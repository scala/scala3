package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.core.Symbols._

trait SymbolOpsImpl extends scala.tasty.reflect.SymbolOps with TastyCoreImpl {

  def SymbolDeco(symbol: Symbol): SymbolAPI = new SymbolAPI {

    def owner(implicit ctx: Context): Symbol = symbol.owner

    def isEmpty(implicit ctx: Context): Boolean = symbol eq NoSymbol
    def isClass(implicit ctx: Context): Boolean = symbol.isClass

    def flags(implicit ctx: Context): FlagSet = new FlagSet(symbol.flags)

    def name(implicit ctx: Context): String = symbol.name.toString
    def fullName(implicit ctx: Context): String = symbol.fullName.toString

    def localContext(implicit ctx: Context): Context = ctx.withOwner(symbol)

    def tree(implicit ctx: Context): Option[Definition] =
      if (isEmpty) None else Some(FromSymbol.definitionFromSym(symbol))

  }

}
