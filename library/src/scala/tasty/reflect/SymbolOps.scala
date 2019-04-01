package scala.tasty
package reflect

/** Tasty reflect symbol */
trait SymbolOps extends Core {

  // Symbol

  implicit class SymbolAPI(self: Symbol) {

    /** Owner of this symbol. The owner is the symbol in which this symbol is defined */
    def owner(implicit ctx: Context): Symbol = kernel.Symbol_owner(self)

    /** Flags of this symbol */
    def flags(implicit ctx: Context): Flags = kernel.Symbol_flags(self)

    /** This symbol is private within the resulting type */
    def privateWithin(implicit ctx: Context): Option[Type] = kernel.Symbol_privateWithin(self)

    /** This symbol is protected within the resulting type */
    def protectedWithin(implicit ctx: Context): Option[Type] = kernel.Symbol_protectedWithin(self)

    /** The name of this symbol */
    def name(implicit ctx: Context): String = kernel.Symbol_name(self)

    /** The full name of this symbol up to the root package */
    def fullName(implicit ctx: Context): String = kernel.Symbol_fullName(self)

    /** The position of this symbol */
    def pos(implicit ctx: Context): Position = kernel.Symbol_pos(self)

    def localContext(implicit ctx: Context): Context = kernel.Symbol_localContext(self)

    /** The comment for this symbol, if any */
    def comment(implicit ctx: Context): Option[Comment] = kernel.Symbol_comment(self)

    /** Unsafe cast as to PackageSymbol. Use IsPackageSymbol to safely check and cast to PackageSymbol */
    def asPackageDef(implicit ctx: Context): PackageDefSymbol = self match {
      case IsPackageDefSymbol(self) => self
      case _ => throw new Exception("not a PackageDefSymbol")
    }

    /** Unsafe cast as to ClassSymbol. Use IsClassDefSymbol to safely check and cast to ClassSymbol */
    def asClassDef(implicit ctx: Context): ClassDefSymbol = self match {
      case IsClassDefSymbol(self) => self
      case _ => throw new Exception("not a ClassDefSymbol")
    }

    /** Unsafe cast as to DefSymbol. Use IsDefDefSymbol to safely check and cast to DefSymbol */
    def asDefDef(implicit ctx: Context): DefDefSymbol = self match {
      case IsDefDefSymbol(self) => self
      case _ => throw new Exception("not a DefDefSymbol")
    }

    /** Unsafe cast as to ValSymbol. Use IsValDefSymbol to safely check and cast to ValSymbol */
    def asValDef(implicit ctx: Context): ValDefSymbol = self match {
      case IsValDefSymbol(self) => self
      case _ => throw new Exception("not a ValDefSymbol")
    }

    /** Unsafe cast as to TypeSymbol. Use IsTypeDefSymbol to safely check and cast to TypeSymbol */
    def asTypeDef(implicit ctx: Context): TypeDefSymbol = self match {
      case IsTypeDefSymbol(self) => self
      case _ => throw new Exception("not a TypeDefSymbol")
    }

    /** Unsafe cast as to BindSymbol. Use IsBindSymbol to safely check and cast to BindSymbol */
    def asBindDef(implicit ctx: Context): BindSymbol = self match {
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

    def isType(implicit ctx: Context): Boolean = kernel.matchTypeSymbol(self).isDefined
    def isTerm(implicit ctx: Context): Boolean = kernel.matchTermSymbol(self).isDefined
  }

  // PackageSymbol

  object IsPackageDefSymbol {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[PackageDefSymbol] =
      kernel.matchPackageDefSymbol(symbol)
  }

  implicit class PackageDefSymbolAPI(self: PackageDefSymbol) {
    def tree(implicit ctx: Context): PackageDef =
      kernel.PackageDefSymbol_tree(self)
  }

  // TypeSymbol

  object IsTypeSymbol {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[TypeSymbol] =
      kernel.matchTypeSymbol(symbol)
  }

  // ClassSymbol

  object IsClassDefSymbol {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[ClassDefSymbol] =
      kernel.matchClassDefSymbol(symbol)
  }

  object ClassDefSymbol {
    /** The ClassSymbol of a global class definition */
    def of(fullName: String)(implicit ctx: Context): ClassDefSymbol =
      kernel.ClassDefSymbol_of(fullName)
  }

  implicit class ClassDefSymbolAPI(self: ClassDefSymbol) {
    /** ClassDef tree of this defintion */
    def tree(implicit ctx: Context): ClassDef =
      kernel.ClassDefSymbol_tree(self)

    /** Fields directly declared in the class */
    def fields(implicit ctx: Context): List[Symbol] =
      kernel.ClassDefSymbol_fields(self)

    /** Field with the given name directly declared in the class */
    def field(name: String)(implicit ctx: Context): Option[Symbol] =
      kernel.ClassDefSymbol_field(self)(name)

    /** Get non-private named methods defined directly inside the class */
    def classMethod(name: String)(implicit ctx: Context): List[DefDefSymbol] =
      kernel.ClassDefSymbol_classMethod(self)(name)

    /** Get all non-private methods defined directly inside the class, exluding constructors */
    def classMethods(implicit ctx: Context): List[DefDefSymbol] =
      kernel.ClassDefSymbol_classMethods(self)

    /** Get named non-private methods declared or inherited */
    def method(name: String)(implicit ctx: Context): List[DefDefSymbol] =
      kernel.ClassDefSymbol_method(self)(name)

    /** Get all non-private methods declared or inherited */
    def methods(implicit ctx: Context): List[DefDefSymbol] =
      kernel.ClassDefSymbol_methods(self)

    /** Fields of a case class type -- only the ones declared in primary constructor */
    def caseFields(implicit ctx: Context): List[ValDefSymbol] =
      kernel.ClassDefSymbol_caseFields(self)

    /** The class symbol of the companion module class */
    def companionClass(implicit ctx: Context): Option[ClassDefSymbol] =
      kernel.ClassDefSymbol_companionClass(self)

    /** The symbol of the companion module */
    def companionModule(implicit ctx: Context): Option[ValDefSymbol] =
      kernel.ClassDefSymbol_companionModule(self)

    /** The symbol of the class of the companion module */
    def moduleClass(implicit ctx: Context): Option[Symbol] =
      kernel.ClassDefSymbol_moduleClass(self)
  }

  // TypeSymbol

  object IsTypeDefSymbol {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[TypeDefSymbol] =
      kernel.matchTypeDefSymbol(symbol)
  }

  implicit class TypeDefSymbolAPI(self: TypeDefSymbol) {
    /** TypeDef tree of this definition */
    def tree(implicit ctx: Context): TypeDef =
      kernel.TypeDefSymbol_tree(self)

    def isTypeParam(implicit ctx: Context): Boolean =
      kernel.TypeDefSymbol_isTypeParam(self)
  }

  // TypeBindSymbol

  object IsTypeBindSymbol {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[TypeBindSymbol] =
      kernel.matchTypeBindSymbol(symbol)
  }

  implicit class TypeBindSymbolAPI(self: TypeBindSymbol) {
    /** TypeBind pattern of this definition */
    def tree(implicit ctx: Context): TypeBind =
      kernel.TypeBindSymbol_tree(self)
  }

  // TermSymbol

  object IsTermSymbol {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[TermSymbol] =
      kernel.matchTermSymbol(symbol)
  }

  // DefSymbol

  object IsDefDefSymbol {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[DefDefSymbol] =
      kernel.matchDefDefSymbol(symbol)
  }

  implicit class DefDefSymbolAPI(self: DefDefSymbol) {
    /** DefDef tree of this defintion */
    def tree(implicit ctx: Context): DefDef =
      kernel.DefDefSymbol_tree(self)

    /** Signature of this defintion */
    def signature(implicit ctx: Context): Signature =
      kernel.DefDefSymbol_signature(self)
  }

  // ValSymbol

  object IsValDefSymbol {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[ValDefSymbol] =
      kernel.matchValDefSymbol(symbol)
  }

  implicit class ValDefSymbolAPI(self: ValDefSymbol) {
    /** ValDef tree of this defintion */
    def tree(implicit ctx: Context): ValDef =
      kernel.ValDefSymbol_tree(self)

    /** The class symbol of the companion module class */
    def moduleClass(implicit ctx: Context): Option[ClassDefSymbol] =
      kernel.ValDefSymbol_moduleClass(self)

    def companionClass(implicit ctx: Context): Option[ClassDefSymbol] =
      kernel.ValDefSymbol_companionClass(self)
  }

  // BindSymbol

  object IsBindSymbol {
    def unapply(symbol: Symbol)(implicit ctx: Context): Option[BindSymbol] =
      kernel.matchBindSymbol(symbol)
  }

  implicit class BindSymbolAPI(self: BindSymbol) {
    /** Bind pattern of this definition */
    def tree(implicit ctx: Context): Bind =
      kernel.BindSymbol_tree(self)
  }

  // NoSymbol

  object NoSymbol {
    def unapply(symbol: Symbol)(implicit ctx: Context): Boolean =
      kernel.matchNoSymbol(symbol)
  }
}
