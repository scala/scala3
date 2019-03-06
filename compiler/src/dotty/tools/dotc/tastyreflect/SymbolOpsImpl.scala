package dotty.tools.dotc
package tastyreflect

import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Decorators._

trait SymbolOpsImpl extends scala.tasty.reflect.SymbolOps with CoreImpl {

  def SymbolDeco(symbol: Symbol): SymbolAPI = new SymbolAPI {

    def flags(implicit ctx: Context): Flags = symbol.flags

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

    def pos(implicit ctx: Context): Position = symbol.sourcePos

    def comment(implicit ctx: Context): Option[Comment] = {
      import dotty.tools.dotc.core.Comments.CommentsContext
      val docCtx = ctx.docCtx.getOrElse {
        throw new RuntimeException(
          "DocCtx could not be found and comments are unavailable. This is a compiler-internal error."
        )
      }
      docCtx.docstring(symbol)
    }

    def owner(implicit ctx: Context): Symbol = symbol.owner

    def isLocalDummy(implicit ctx: Context): Boolean = symbol.isLocalDummy
    def isRefinementClass(implicit ctx: Context): Boolean = symbol.isRefinementClass
    def isAliasType(implicit ctx: Context): Boolean = symbol.isAliasType
    def isAnonymousClass(implicit ctx: Context): Boolean = symbol.isAnonymousClass
    def isAnonymousFunction(implicit ctx: Context): Boolean = symbol.isAnonymousFunction
    def isAbstractType(implicit ctx: Context): Boolean = symbol.isAbstractType
    def isClassConstructor(implicit ctx: Context): Boolean = symbol.isClassConstructor

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

    def isDefinedInCurrentRun(implicit ctx: Context): Boolean = {
      symbol.topLevelClass.asClass.isDefinedInCurrentRun
    }

  }

  object IsPackageSymbol extends IsPackageSymbolModule {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[PackageSymbol] =
      if (symbol.is(Flags.Package)) Some(symbol) else None
  }

  def PackageSymbolDeco(symbol: PackageSymbol): PackageSymbolAPI = new PackageSymbolAPI {
    def tree(implicit ctx: Context): PackageDef = FromSymbol.packageDefFromSym(symbol)
  }

  object IsTypeSymbol extends IsTypeSymbolModule {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[TypeSymbol] =
      if (symbol.isType) Some(symbol.asType) else None
  }

  def TypeSymbolDeco(symbol: TypeSymbol): TypeSymbolAPI = new TypeSymbolAPI {
    def tree(implicit ctx: Context): TypeDef = FromSymbol.typeDefFromSym(symbol)

    def isTypeParam(implicit ctx: Context): Boolean = symbol.isTypeParam
  }

  object ClassSymbol extends ClassSymbolModule {
    def of(fullName: String)(implicit ctx: Context): ClassSymbol = ctx.requiredClass(fullName)
  }

  object IsClassSymbol extends IsClassSymbolModule {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[ClassSymbol] =
      if (symbol.isClass) Some(symbol.asClass) else None
  }

  def ClassSymbolDeco(symbol: ClassSymbol): ClassSymbolAPI = new ClassSymbolAPI {
    def tree(implicit ctx: Context): ClassDef = FromSymbol.classDef(symbol)

    def fields(implicit ctx: Context): List[Symbol] = {
      symbol.unforcedDecls.filter(isField)
    }

    def field(name: String)(implicit ctx: Context): Option[Symbol] = {
      val sym = symbol.unforcedDecls.find(sym => sym.name == name.toTermName)
      if (sym.exists && isField(sym)) Some(sym) else None
    }

    def classMethod(name: String)(implicit ctx: Context): List[DefSymbol] = {
      symbol.typeRef.decls.iterator.collect {
        case sym if isMethod(sym) && sym.name.toString == name => sym.asTerm
      }.toList
    }

    def classMethods(implicit ctx: Context): List[DefSymbol] = {
      symbol.typeRef.decls.iterator.collect {
        case sym if isMethod(sym) => sym.asTerm
      }.toList
    }

    def method(name: String)(implicit ctx: Context): List[DefSymbol] = {
       symbol.typeRef.allMembers.iterator.map(_.symbol).collect {
        case sym if isMethod(sym) && sym.name.toString == name => sym.asTerm
      }.toList
    }

    def methods(implicit ctx: Context): List[DefSymbol] = {
      symbol.typeRef.allMembers.iterator.map(_.symbol).collect {
        case sym if isMethod(sym) => sym.asTerm
      }.toList
    }

    private def isMethod(sym: Symbol)(implicit ctx: Context): Boolean =
      sym.isTerm && sym.is(Flags.Method) && !sym.isConstructor

    def caseFields(implicit ctx: Context): List[ValSymbol] = {
      if (!symbol.isClass) Nil
      else symbol.asClass.paramAccessors.collect {
        case sym if sym.is(Flags.CaseAccessor) => sym.asTerm
      }
    }

    def companionClass(implicit ctx: Context): Option[ClassSymbol] = {
      val sym = symbol.companionModule.companionClass
      if (sym.exists) Some(sym.asClass) else None
    }

    def companionModule(implicit ctx: Context): Option[ValSymbol] = {
      val sym = symbol.companionModule
      if (sym.exists) Some(sym.asTerm) else None
    }

    def moduleClass(implicit ctx: Context): Option[Symbol] = {
      val sym = symbol.moduleClass
      if (sym.exists) Some(sym.asTerm) else None
    }

    private def isField(sym: Symbol)(implicit ctx: Context): Boolean = sym.isTerm && !sym.is(Flags.Method)
  }

  object IsDefSymbol extends IsDefSymbolModule {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[DefSymbol] =
      if (symbol.isTerm && symbol.is(Flags.Method)) Some(symbol.asTerm) else None
  }

  def DefSymbolDeco(symbol: DefSymbol): DefSymbolAPI = new DefSymbolAPI {
    def tree(implicit ctx: Context): DefDef = FromSymbol.defDefFromSym(symbol)

    def signature(implicit ctx: Context): Signature = {
      symbol.signature
    }
  }

  object IsValSymbol extends IsValSymbolModule {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[ValSymbol] =
      if (symbol.isTerm && !symbol.is(Flags.Method) && !symbol.is(Flags.Case)) Some(symbol.asTerm) else None
  }

  def ValSymbolDeco(symbol: ValSymbol): ValSymbolAPI = new ValSymbolAPI {
    def tree(implicit ctx: Context): ValDef = FromSymbol.valDefFromSym(symbol)

    def moduleClass(implicit ctx: Context): Option[ClassSymbol] = {
      val sym = symbol.moduleClass
      if (sym.exists) Some(sym.asClass) else None
    }

    def companionClass(implicit ctx: Context): Option[ClassSymbol] = {
      val sym = symbol.companionClass
      if (sym.exists) Some(sym.asClass) else None
    }
  }

  object IsBindSymbol extends IsBindSymbolModule {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[BindSymbol] =
      if (symbol.isTerm && symbol.is(Flags.Case)) Some(symbol.asTerm) else None // TODO
  }

  def BindSymbolDeco(symbol: BindSymbol): BindSymbolAPI = new BindSymbolAPI {
    def tree(implicit ctx: Context): Bind = FromSymbol.bindFromSym(symbol)
  }

  object NoSymbol extends NoSymbolModule {
    def unapply(symbol: Symbol)(implicit ctx: Context): Boolean = symbol ne core.Symbols.NoSymbol
  }
}
