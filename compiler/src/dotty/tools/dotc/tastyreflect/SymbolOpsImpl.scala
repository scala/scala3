package dotty.tools.dotc
package tastyreflect

import dotty.tools.dotc.core.Symbols._

trait SymbolOpsImpl extends scala.tasty.reflect.SymbolOps with TastyCoreImpl {

  def SymbolDeco(symbol: Symbol): SymbolAPI = new SymbolAPI {

    def isEmpty(implicit ctx: Context): Boolean = symbol eq NoSymbol
    def isClass(implicit ctx: Context): Boolean = symbol.isClass

    def flags(implicit ctx: Context): FlagSet = new FlagSet(symbol.flags)

    def privateWithin(implicit ctx: Context): Option[Type] = {
      val within = symbol.privateWithin
      if (within.exists && !symbol.is(core.Flags.Protected)) Some(within.typeRef)
      else None
    }

    def protectedWithin(implicit ctx: Context): Option[Type] = {
      val within = symbol.privateWithin
      if (within.exists && symbol.is(core.Flags.Protected)) Some(within.typeRef)
      else None
    }

    def name(implicit ctx: Context): String = symbol.name.toString
    def fullName(implicit ctx: Context): String = symbol.fullName.toString

    def owner(implicit ctx: Context): Symbol = symbol.owner

    def localContext(implicit ctx: Context): Context = {
      if (symbol.exists) ctx.withOwner(symbol)
      else ctx
    }

    def tree(implicit ctx: Context): Option[Definition] =
      if (isEmpty) None else Some(FromSymbol.definitionFromSym(symbol))

    def annots(implicit ctx: Context): List[Term] = {
      symbol.annotations.flatMap {
        case _: core.Annotations.LazyBodyAnnotation => Nil
        case annot => annot.tree :: Nil
      }
    }

  }

}
