package dotty.tools.dotc
package tastyreflect

import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols._

trait SymbolOpsImpl extends scala.tasty.reflect.SymbolOps with TastyCoreImpl {

  def SymbolDeco(symbol: Symbol): SymbolAPI = new SymbolAPI {

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

    def asPackage(implicit ctx: Context): PackageSymbol = symbol match {
      case IsPackageSymbol(symbol) => symbol
      case _ => throw new Exception("not a PackageSymbol")
    }

    def asClass(implicit ctx: Context): ClassSymbol = symbol match {
      case IsClassSymbol(symbol) => symbol.asClass
      case _ => throw new Exception("not a ClassSymbol")
    }

    def asDef(implicit ctx: Context): DefSymbol = symbol match {
      case IsDefSymbol(symbol) => symbol.asTerm
      case _ => throw new Exception("not a DefSymbol")
    }

    def asVal(implicit ctx: Context): ValSymbol = symbol match {
      case IsValSymbol(symbol) => symbol
      case _ => throw new Exception("not a ValSymbol")
    }

    def asBind(implicit ctx: Context): BindSymbol = symbol match {
      case IsBindSymbol(symbol) => symbol
      case _ => throw new Exception("not a BindSymbol")
    }

    def asType(implicit ctx: Context): TypeSymbol = symbol match {
      case IsTypeSymbol(symbol) => symbol.asType
      case _ => throw new Exception("not a TypeSymbol")
    }

    def treeOpt(implicit ctx: Context): Option[Definition] =
      if (symbol eq core.Symbols.NoSymbol) None
      else Some(FromSymbol.definitionFromSym(symbol))

    def annots(implicit ctx: Context): List[Term] = {
      symbol.annotations.flatMap {
        case _: core.Annotations.LazyBodyAnnotation => Nil
        case annot => annot.tree :: Nil
      }
    }

  }

  object IsPackageSymbol extends IsPackageSymbolExtractor {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[PackageSymbol] =
      if (symbol.is(Flags.Package)) Some(symbol) else None
  }

  def PackageSymbolDeco(symbol: PackageSymbol): PackageSymbolAPI = new PackageSymbolAPI {
    def tree(implicit ctx: Context): PackageDef = FromSymbol.packageDefFromSym(symbol)
  }

  object IsTypeSymbol extends IsTypeSymbolExtractor {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[TypeSymbol] =
      if (symbol.isType) Some(symbol.asType) else None
  }

  def TypeSymbolDeco(symbol: TypeSymbol): TypeSymbolAPI = new TypeSymbolAPI {
    def tree(implicit ctx: Context): TypeDef = FromSymbol.typeDefFromSym(symbol)
  }

  object IsClassSymbol extends IsClassSymbolExtractor {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[ClassSymbol] =
      if (symbol.isClass) Some(symbol.asClass) else None
  }

  def ClassSymbolDeco(symbol: ClassSymbol): ClassSymbolAPI = new ClassSymbolAPI {
    def tree(implicit ctx: Context): ClassDef = FromSymbol.classDef(symbol)
  }

  object IsDefSymbol extends IsDefSymbolExtractor {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[DefSymbol] =
      if (symbol.isTerm && symbol.is(Flags.Method)) Some(symbol.asTerm) else None
  }

  def DefSymbolDeco(symbol: DefSymbol): DefSymbolAPI = new DefSymbolAPI {
    def tree(implicit ctx: Context): DefDef = FromSymbol.defDefFromSym(symbol)
  }

  object IsValSymbol extends IsValSymbolExtractor {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[ValSymbol] =
      if (symbol.isTerm && !symbol.is(Flags.Method) && !symbol.is(Flags.Case)) Some(symbol.asTerm) else None
  }

  def ValSymbolDeco(symbol: ValSymbol): ValSymbolAPI = new ValSymbolAPI {
    def tree(implicit ctx: Context): ValDef = FromSymbol.valDefFromSym(symbol)
  }

  object IsBindSymbol extends IsBindSymbolExtractor {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[BindSymbol] =
      if (symbol.isTerm && symbol.is(Flags.Case)) Some(symbol.asTerm) else None // TODO
  }

  def BindSymbolDeco(symbol: BindSymbol): BindSymbolAPI = new BindSymbolAPI {
    def tree(implicit ctx: Context): Bind = FromSymbol.bindFromSym(symbol)
  }

  object NoSymbol extends NoSymbolExtractor {
    def unapply(symbol: Symbol)(implicit ctx: Context): Boolean = symbol ne core.Symbols.NoSymbol
  }
}
