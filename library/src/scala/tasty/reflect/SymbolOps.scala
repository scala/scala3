package scala.tasty
package reflect

/** Tasty reflect symbol */
trait SymbolOps extends Core {

  // Symbol

  implicit class SymbolDeco(self: Symbol) {

    /** Owner of this symbol. The owner is the symbol in which this symbol is defined. */
    def owner(implicit ctx: Context): Symbol = kernel.Symbol_owner(self)

    /** Flags of this symbol */
    def flags(implicit ctx: Context): Flags = kernel.Symbol_flags(self)

    /** This symbol is private within the resulting type. */
    def privateWithin(implicit ctx: Context): Option[Type] = kernel.Symbol_privateWithin(self)

    /** This symbol is protected within the resulting type. */
    def protectedWithin(implicit ctx: Context): Option[Type] = kernel.Symbol_protectedWithin(self)

    /** The name of this symbol. */
    def name(implicit ctx: Context): String = kernel.Symbol_name(self)

    /** The full name of this symbol up to the root package. */
    def fullName(implicit ctx: Context): String = kernel.Symbol_fullName(self)

    /** The position of this symbol */
    def pos(implicit ctx: Context): Position = kernel.Symbol_pos(self)

    def localContext(implicit ctx: Context): Context = kernel.Symbol_localContext(self)

    /** The comment for this symbol, if any */
    def comment(implicit ctx: Context): Option[Comment] = kernel.Symbol_comment(self)

    /** Unsafe cast as to PackageSymbol. Use IsPackageSymbol to safly check and cast to PackageSymbol */
    def asPackage(implicit ctx: Context): PackageSymbol = self match {
      case IsPackageSymbol(self) => self
      case _ => throw new Exception("not a PackageSymbol")
    }

    /** Unsafe cast as to ClassSymbol. Use IsClassSymbol to safly check and cast to ClassSymbol */
    def asClass(implicit ctx: Context): ClassSymbol = self match {
      case IsClassSymbol(self) => self
      case _ => throw new Exception("not a ClassSymbol")
    }

    /** Unsafe cast as to DefSymbol. Use IsDefSymbol to safly check and cast to DefSymbol */
    def asDef(implicit ctx: Context): DefSymbol = self match {
      case IsDefSymbol(self) => self
      case _ => throw new Exception("not a DefSymbol")
    }

    /** Unsafe cast as to ValSymbol. Use IsValSymbol to safly check and cast to ValSymbol */
    def asVal(implicit ctx: Context): ValSymbol = self match {
      case IsValSymbol(self) => self
      case _ => throw new Exception("not a ValSymbol")
    }

    /** Unsafe cast as to TypeSymbol. Use IsTypeSymbol to safly check and cast to TypeSymbol */
    def asType(implicit ctx: Context): TypeSymbol = self match {
      case IsTypeSymbol(self) => self
      case _ => throw new Exception("not a TypeSymbol")
    }

    /** Unsafe cast as to BindSymbol. Use IsBindSymbol to safly check and cast to BindSymbol */
    def asBind(implicit ctx: Context): BindSymbol = self match {
      case IsBindSymbol(self) => self
      case _ => throw new Exception("not a BindSymbol")
    }

    /** Annotations attached to this symbol */
    def annots(implicit ctx: Context): List[Term] = kernel.Symbol_annots(self)

    def isDefinedInCurrentRun(implicit ctx: Context): Boolean = kernel.Symbol_isDefinedInCurrentRun(self)

    def isLocalDummy(implicit ctx: Context): Boolean = kernel.Symbol_isLocalDummy(self)
    def isRefinementClass(implicit ctx: Context): Boolean = kernel.Symbol_isRefinementClass(self)
    def isAliasType(implicit ctx: Context): Boolean = kernel.Symbol_isAliasType(self)
    def isAnonymousClass(implicit ctx: Context): Boolean = kernel.Symbol_isAnonymousClass(self)
    def isAnonymousFunction(implicit ctx: Context): Boolean = kernel.Symbol_isAnonymousFunction(self)
    def isAbstractType(implicit ctx: Context): Boolean = kernel.Symbol_isAbstractType(self)
    def isClassConstructor(implicit ctx: Context): Boolean = kernel.Symbol_isClassConstructor(self)
  }

  // PackageSymbol

  val IsPackageSymbol: IsPackageSymbolModule
  abstract class IsPackageSymbolModule {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[PackageSymbol]
  }

  trait PackageSymbolAPI {
    def tree(implicit ctx: Context): PackageDef
  }
  implicit def PackageSymbolDeco(symbol: PackageSymbol): PackageSymbolAPI

  // ClassSymbol

  val IsClassSymbol: IsClassSymbolModule
  abstract class IsClassSymbolModule {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[ClassSymbol]
  }

  val ClassSymbol: ClassSymbolModule
  abstract class ClassSymbolModule {
    /** The ClassSymbol of a global class definition */
    def of(fullName: String)(implicit ctx: Context): ClassSymbol
  }

  trait ClassSymbolAPI {
    /** ClassDef tree of this defintion. */
    def tree(implicit ctx: Context): ClassDef

    /** Fields directly declared in the class */
    def fields(implicit ctx: Context): List[Symbol]

    /** Field with the given name directly declared in the class */
    def field(name: String)(implicit ctx: Context): Option[Symbol]

    /** Get non-private named methods defined directly inside the class */
    def classMethod(name: String)(implicit ctx: Context): List[DefSymbol]

    /** Get all non-private methods defined directly inside the class, exluding constructors */
    def classMethods(implicit ctx: Context): List[DefSymbol]

    /** Get named non-private methods declared or inherited */
    def method(name: String)(implicit ctx: Context): List[DefSymbol]

    /** Get all non-private methods declared or inherited */
    def methods(implicit ctx: Context): List[DefSymbol]

    /** Fields of a case class type -- only the ones declared in primary constructor */
    def caseFields(implicit ctx: Context): List[ValSymbol]

    /** The class symbol of the companion module class */
    def companionClass(implicit ctx: Context): Option[ClassSymbol]

    /** The symbol of the companion module */
    def companionModule(implicit ctx: Context): Option[ValSymbol]

    def moduleClass(implicit ctx: Context): Option[Symbol]
  }
  implicit def ClassSymbolDeco(symbol: ClassSymbol): ClassSymbolAPI

  // TypeSymbol

  val IsTypeSymbol: IsTypeSymbolModule
  abstract class IsTypeSymbolModule {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[TypeSymbol]
  }

  trait TypeSymbolAPI {
    /** TypeDef tree of this definition. */
    def tree(implicit ctx: Context): TypeDef

    def isTypeParam(implicit ctx: Context): Boolean
  }
  implicit def TypeSymbolDeco(symbol: TypeSymbol): TypeSymbolAPI

  // DefSymbol

  val IsDefSymbol: IsDefSymbolModule
  abstract class IsDefSymbolModule {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[DefSymbol]
  }

  trait DefSymbolAPI {
    /** DefDef tree of this defintion. */
    def tree(implicit ctx: Context): DefDef

    def signature(implicit ctx: Context): Signature
  }
  implicit def DefSymbolDeco(symbol: DefSymbol): DefSymbolAPI

  // ValSymbol

  val IsValSymbol: IsValSymbolModule
  abstract class IsValSymbolModule {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[ValSymbol]
  }

  trait ValSymbolAPI {
    /** ValDef tree of this defintion. */
    def tree(implicit ctx: Context): ValDef

    /** The class symbol of the companion module class */
    def moduleClass(implicit ctx: Context): Option[ClassSymbol]

    def companionClass(implicit ctx: Context): Option[ClassSymbol]
  }
  implicit def ValSymbolDeco(symbol: ValSymbol): ValSymbolAPI

  // BindSymbol

  val IsBindSymbol: IsBindSymbolModule
  abstract class IsBindSymbolModule {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[BindSymbol]
  }

  trait BindSymbolAPI {
    /** Bind pattern of this definition. */
    def tree(implicit ctx: Context): Bind
  }
  implicit def BindSymbolDeco(symbol: BindSymbol): BindSymbolAPI

  // NoSymbol

  val NoSymbol: NoSymbolModule
  abstract class NoSymbolModule {
    def unapply(symbol: Symbol)(implicit ctx: Context): Boolean
  }
}
