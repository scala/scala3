package scala.tasty
package reflect

/** Tasty reflect symbol */
trait SymbolOps extends Core {

  // Symbol

  implicit class SymbolAPI(self: Symbol) {

    /** Owner of this symbol. The owner is the symbol in which this symbol is defined */
    def owner(given ctx: Context): Symbol = internal.Symbol_owner(self)

    /** Flags of this symbol */
    def flags(given ctx: Context): Flags = internal.Symbol_flags(self)

    /** This symbol is private within the resulting type */
    def privateWithin(given ctx: Context): Option[Type] = internal.Symbol_privateWithin(self)

    /** This symbol is protected within the resulting type */
    def protectedWithin(given ctx: Context): Option[Type] = internal.Symbol_protectedWithin(self)


    /** The name of this symbol */
    def name(given ctx: Context): String = internal.Symbol_name(self)

    /** The full name of this symbol up to the root package */
    def fullName(given ctx: Context): String = internal.Symbol_fullName(self)

    /** The position of this symbol */
    def pos(given ctx: Context): Position = internal.Symbol_pos(self)

    def localContext(given ctx: Context): Context = internal.Symbol_localContext(self)

    /** The comment for this symbol, if any */
    def comment(given ctx: Context): Option[Comment] = internal.Symbol_comment(self)

    /** Unsafe cast as to PackageSymbol. Use IsPackageSymbol to safely check and cast to PackageSymbol */
    def asPackageDef(given ctx: Context): PackageDefSymbol = self match {
      case IsPackageDefSymbol(self) => self
      case _ => throw new Exception("not a PackageDefSymbol")
    }

    /** Unsafe cast as to ClassSymbol. Use IsClassDefSymbol to safely check and cast to ClassSymbol */
    def asClassDef(given ctx: Context): ClassDefSymbol = self match {
      case IsClassDefSymbol(self) => self
      case _ => throw new Exception("not a ClassDefSymbol")
    }

    /** Unsafe cast as to DefSymbol. Use IsDefDefSymbol to safely check and cast to DefSymbol */
    def asDefDef(given ctx: Context): DefDefSymbol = self match {
      case IsDefDefSymbol(self) => self
      case _ => throw new Exception("not a DefDefSymbol")
    }

    /** Unsafe cast as to ValSymbol. Use IsValDefSymbol to safely check and cast to ValSymbol */
    def asValDef(given ctx: Context): ValDefSymbol = self match {
      case IsValDefSymbol(self) => self
      case _ => throw new Exception("not a ValDefSymbol")
    }

    /** Unsafe cast as to TypeSymbol. Use IsTypeDefSymbol to safely check and cast to TypeSymbol */
    def asTypeDef(given ctx: Context): TypeDefSymbol = self match {
      case IsTypeDefSymbol(self) => self
      case _ => throw new Exception("not a TypeDefSymbol")
    }

    /** Unsafe cast as to BindSymbol. Use IsBindSymbol to safely check and cast to BindSymbol */
    def asBindDef(given ctx: Context): BindSymbol = self match {
      case IsBindSymbol(self) => self
      case _ => throw new Exception("not a BindSymbol")
    }

    /** Tree of this definition */
    def tree(given ctx: Context): Tree =
      internal.Symbol_tree(self)

    /** Pattern of this definition */
    def pattern(given ctx: Context): Pattern =
      internal.Symbol_pattern(self)

    /** Annotations attached to this symbol */
    def annots(given ctx: Context): List[Term] = internal.Symbol_annots(self)

    def isDefinedInCurrentRun(given ctx: Context): Boolean = internal.Symbol_isDefinedInCurrentRun(self)

    def isLocalDummy(given ctx: Context): Boolean = internal.Symbol_isLocalDummy(self)
    def isRefinementClass(given ctx: Context): Boolean = internal.Symbol_isRefinementClass(self)
    def isAliasType(given ctx: Context): Boolean = internal.Symbol_isAliasType(self)
    def isAnonymousClass(given ctx: Context): Boolean = internal.Symbol_isAnonymousClass(self)
    def isAnonymousFunction(given ctx: Context): Boolean = internal.Symbol_isAnonymousFunction(self)
    def isAbstractType(given ctx: Context): Boolean = internal.Symbol_isAbstractType(self)
    def isClassConstructor(given ctx: Context): Boolean = internal.Symbol_isClassConstructor(self)

    def isType(given ctx: Context): Boolean = internal.matchTypeSymbol(self).isDefined
    def isTerm(given ctx: Context): Boolean = internal.matchTermSymbol(self).isDefined
    def isValDef(given ctx: Context): Boolean = internal.matchValDefSymbol(self).isDefined
    def isDefDef(given ctx: Context): Boolean = internal.matchDefDefSymbol(self).isDefined
    def isClass(given ctx: Context): Boolean = internal.matchClassDefSymbol(self).isDefined
  }

  // PackageSymbol

  object IsPackageDefSymbol {
    def unapply(symbol: Symbol)(given ctx: Context): Option[PackageDefSymbol] =
      internal.matchPackageDefSymbol(symbol)
  }

  // TypeSymbol

  object IsTypeSymbol {
    def unapply(symbol: Symbol)(given ctx: Context): Option[TypeSymbol] =
      internal.matchTypeSymbol(symbol)
  }

  // ClassSymbol

  object IsClassDefSymbol {
    def unapply(symbol: Symbol)(given ctx: Context): Option[ClassDefSymbol] =
      internal.matchClassDefSymbol(symbol)
  }

  object ClassDefSymbol {
    /** The ClassSymbol of a global class definition */
    def of(fullName: String)(given ctx: Context): ClassDefSymbol =
      internal.ClassDefSymbol_of(fullName)
  }

  implicit class ClassDefSymbolAPI(self: ClassDefSymbol) {

    /** Fields directly declared in the class */
    def fields(given ctx: Context): List[Symbol] =
      internal.ClassDefSymbol_fields(self)

    /** Field with the given name directly declared in the class */
    def field(name: String)(given ctx: Context): Option[Symbol] =
      internal.ClassDefSymbol_field(self)(name)

    /** Get non-private named methods defined directly inside the class */
    def classMethod(name: String)(given ctx: Context): List[DefDefSymbol] =
      internal.ClassDefSymbol_classMethod(self)(name)

    /** Get all non-private methods defined directly inside the class, exluding constructors */
    def classMethods(given ctx: Context): List[DefDefSymbol] =
      internal.ClassDefSymbol_classMethods(self)

    /** Get named non-private methods declared or inherited */
    def method(name: String)(given ctx: Context): List[DefDefSymbol] =
      internal.ClassDefSymbol_method(self)(name)

    /** Get all non-private methods declared or inherited */
    def methods(given ctx: Context): List[DefDefSymbol] =
      internal.ClassDefSymbol_methods(self)

    /** Fields of a case class type -- only the ones declared in primary constructor */
    def caseFields(given ctx: Context): List[ValDefSymbol] =
      internal.ClassDefSymbol_caseFields(self)

    /** The class symbol of the companion module class */
    def companionClass(given ctx: Context): Option[ClassDefSymbol] =
      internal.ClassDefSymbol_companionClass(self)

    /** The symbol of the companion module */
    def companionModule(given ctx: Context): Option[ValDefSymbol] =
      internal.ClassDefSymbol_companionModule(self)

    /** The symbol of the class of the companion module */
    def moduleClass(given ctx: Context): Option[Symbol] =
      internal.ClassDefSymbol_moduleClass(self)
  }

  // TypeSymbol

  object IsTypeDefSymbol {
    def unapply(symbol: Symbol)(given ctx: Context): Option[TypeDefSymbol] =
      internal.matchTypeDefSymbol(symbol)
  }

  implicit class TypeDefSymbolAPI(self: TypeDefSymbol) {
    def isTypeParam(given ctx: Context): Boolean =
      internal.TypeDefSymbol_isTypeParam(self)
  }

  // TypeBindSymbol

  object IsTypeBindSymbol {
    def unapply(symbol: Symbol)(given ctx: Context): Option[TypeBindSymbol] =
      internal.matchTypeBindSymbol(symbol)
  }

  // TermSymbol

  object IsTermSymbol {
    def unapply(symbol: Symbol)(given ctx: Context): Option[TermSymbol] =
      internal.matchTermSymbol(symbol)
  }

  // DefSymbol

  object IsDefDefSymbol {
    def unapply(symbol: Symbol)(given ctx: Context): Option[DefDefSymbol] =
      internal.matchDefDefSymbol(symbol)
  }

  implicit class DefDefSymbolAPI(self: DefDefSymbol) {
    /** Signature of this defintion */
    def signature(given ctx: Context): Signature =
      internal.DefDefSymbol_signature(self)
  }

  // ValSymbol

  object IsValDefSymbol {
    def unapply(symbol: Symbol)(given ctx: Context): Option[ValDefSymbol] =
      internal.matchValDefSymbol(symbol)
  }

  implicit class ValDefSymbolAPI(self: ValDefSymbol) {
    /** The class symbol of the companion module class */
    def moduleClass(given ctx: Context): Option[ClassDefSymbol] =
      internal.ValDefSymbol_moduleClass(self)

    def companionClass(given ctx: Context): Option[ClassDefSymbol] =
      internal.ValDefSymbol_companionClass(self)
  }

  // BindSymbol

  object IsBindSymbol {
    def unapply(symbol: Symbol)(given ctx: Context): Option[BindSymbol] =
      internal.matchBindSymbol(symbol)
  }

  // NoSymbol

  object NoSymbol {
    def unapply(symbol: Symbol)(given ctx: Context): Boolean =
      internal.matchNoSymbol(symbol)
  }
}
