package scala.tasty
package reflect

/** Tasty reflect symbol */
trait SymbolOps extends Core {

  // Symbol

  object Symbol {
    /** The ClassSymbol of a global class definition */
    def classSymbol(fullName: String)(given ctx: Context): ClassDefSymbol =
      internal.Symbol_of(fullName)

    def noSymbol(given ctx: Context): Symbol =
      internal.Symbol_noSymbol
  }

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

    /** Unsafe cast as to PackageSymbol */
    def asPackageDef(given ctx: Context): Symbol =
      if self.isPackageDef then self
      else throw new Exception("not a PackageDefSymbol")

    /** Unsafe cast as to ClassSymbol */
    def asClassDef(given ctx: Context): ClassDefSymbol =
      if self.isClassDef then self.asInstanceOf[ClassDefSymbol]
      else throw new Exception("not a ClassDefSymbol")

    /** Unsafe cast as to DefSymbol */
    def asDefDef(given ctx: Context): DefDefSymbol =
      if self.isDefDef then self.asInstanceOf[DefDefSymbol]
      else throw new Exception("not a DefDefSymbol")

    /** Unsafe cast as to ValSymbol */
    def asValDef(given ctx: Context): ValDefSymbol =
      if self.isValDef then self.asInstanceOf[ValDefSymbol]
      else throw new Exception("not a ValDefSymbol")

    /** Unsafe cast as to TypeSymbol */
    def asTypeDef(given ctx: Context): TypeDefSymbol =
      if self.isTypeDef then self.asInstanceOf[TypeDefSymbol]
      else throw new Exception("not a TypeDefSymbol")

    /** Unsafe cast as to BindSymbol */
    def asBindDef(given ctx: Context): BindSymbol =
      if self.isBind then self.asInstanceOf[BindSymbol]
      else throw new Exception("not a BindSymbol")

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

    def isType(given ctx: Context): Boolean = internal.isTypeSymbol(self)
    def isTerm(given ctx: Context): Boolean = internal.isTermSymbol(self)
    def isValDef(given ctx: Context): Boolean = internal.isValDefSymbol(self)
    def isDefDef(given ctx: Context): Boolean = internal.isDefDefSymbol(self)
    def isTypeDef(given ctx: Context): Boolean = internal.isTypeDefSymbol(self)
    def isClassDef(given ctx: Context): Boolean = internal.isClassDefSymbol(self)
    def isBind(given ctx: Context): Boolean = internal.isBindSymbol(self)
    def isPackageDef(given ctx: Context): Boolean = internal.isPackageDefSymbol(self)
    def isNoSymbol(given ctx: Context): Boolean = internal.isNoSymbol(self)

    /** Fields directly declared in the class */
    def fields(given ctx: Context): List[Symbol] =
      internal.Symbol_fields(self)

    /** Field with the given name directly declared in the class */
    def field(name: String)(given ctx: Context): Symbol =
      internal.Symbol_field(self)(name)

    /** Get non-private named methods defined directly inside the class */
    def classMethod(name: String)(given ctx: Context): List[DefDefSymbol] =
      internal.Symbol_classMethod(self)(name)

    /** Get all non-private methods defined directly inside the class, exluding constructors */
    def classMethods(given ctx: Context): List[DefDefSymbol] =
      internal.Symbol_classMethods(self)

    /** Get named non-private methods declared or inherited */
    def method(name: String)(given ctx: Context): List[DefDefSymbol] =
      internal.Symbol_method(self)(name)

    /** Get all non-private methods declared or inherited */
    def methods(given ctx: Context): List[DefDefSymbol] =
      internal.Symbol_methods(self)

    /** Fields of a case class type -- only the ones declared in primary constructor */
    def caseFields(given ctx: Context): List[ValDefSymbol] =
      internal.Symbol_caseFields(self)

    def isTypeParam(given ctx: Context): Boolean =
      internal.Symbol_isTypeParam(self)

    /** Signature of this defintion */
    def signature(given ctx: Context): Signature =
      internal.Symbol_signature(self)

    /** The class symbol of the companion module class */
    def moduleClass(given ctx: Context): Symbol =
      internal.Symbol_moduleClass(self)

    /** The symbol of the companion class */
    def companionClass(given ctx: Context): Symbol =
      internal.Symbol_companionClass(self)

    /** The symbol of the companion module */
    def companionModule(given ctx: Context): Symbol =
      internal.Symbol_companionModule(self)
  }

}
