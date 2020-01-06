package scala.tasty
package reflect

/** Tasty reflect symbol */
trait SymbolOps extends Core { selfSymbolOps: FlagsOps =>

  object Symbol {
    /** The class Symbol of a global class definition */
    def classSymbol(fullName: String)(given ctx: Context): Symbol =
      internal.Symbol_of(fullName)

    /** Definition not available */
    def noSymbol(given ctx: Context): Symbol =
      internal.Symbol_noSymbol
  }

  given symbolOps: extension (self: Symbol) {

    /** Owner of this symbol. The owner is the symbol in which this symbol is defined. Throws if this symbol does not have an owner. */
    def owner(given ctx: Context): Symbol = internal.Symbol_owner(self)

    /** Owner of this symbol. The owner is the symbol in which this symbol is defined. Returns `NoSymbol` if this symbol does not have an owner. */
    def maybeOwner(given ctx: Context): Symbol = internal.Symbol_maybeOwner(self)

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

    /** Tree of this definition
     *
     * if this symbol `isPackageDef` it will return a `PackageDef`,
     * if this symbol `isClassDef` it will return a `ClassDef`,
     * if this symbol `isTypeDef` it will return a `TypeDef`,
     * if this symbol `isValDef` it will return a `ValDef`,
     * if this symbol `isDefDef` it will return a `DefDef`
     * if this symbol `isBind` it will return a `Bind`
     */
    def tree(given ctx: Context): Tree =
      internal.Symbol_tree(self)

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

    /** Is this the definition of a type? */
    def isType(given ctx: Context): Boolean = internal.Symbol_isType(self)

    /** Is this the definition of a term? */
    def isTerm(given ctx: Context): Boolean = internal.Symbol_isTerm(self)

    /** Is this the definition of a PackageDef tree? */
    def isPackageDef(given ctx: Context): Boolean = internal.Symbol_isPackageDef(self)

    /** Is this the definition of a ClassDef tree? */
    def isClassDef(given ctx: Context): Boolean = internal.Symbol_isClassDef(self)

    /** Is this the definition of a TypeDef tree */
    def isTypeDef(given ctx: Context): Boolean = internal.Symbol_isTypeDef(self)

    /** Is this the definition of a ValDef tree? */
    def isValDef(given ctx: Context): Boolean = internal.Symbol_isValDef(self)

    /** Is this the definition of a DefDef tree? */
    def isDefDef(given ctx: Context): Boolean = internal.Symbol_isDefDef(self)

    /** Is this the definition of a Bind pattern? */
    def isBind(given ctx: Context): Boolean = internal.Symbol_isBind(self)

    /** Does this symbol represent a no definition? */
    def isNoSymbol(given ctx: Context): Boolean = self == Symbol.noSymbol

    /** Does this symbol represent a definition? */
    def exists(given ctx: Context): Boolean = self != Symbol.noSymbol

    /** Fields directly declared in the class */
    def fields(given ctx: Context): List[Symbol] =
      internal.Symbol_fields(self)

    /** Field with the given name directly declared in the class */
    def field(name: String)(given ctx: Context): Symbol =
      internal.Symbol_field(self)(name)

    /** Get non-private named methods defined directly inside the class */
    def classMethod(name: String)(given ctx: Context): List[Symbol] =
      internal.Symbol_classMethod(self)(name)

    /** Get all non-private methods defined directly inside the class, exluding constructors */
    def classMethods(given ctx: Context): List[Symbol] =
      internal.Symbol_classMethods(self)

    /** Get named non-private methods declared or inherited */
    def method(name: String)(given ctx: Context): List[Symbol] =
      internal.Symbol_method(self)(name)

    /** Get all non-private methods declared or inherited */
    def methods(given ctx: Context): List[Symbol] =
      internal.Symbol_methods(self)

    /** Fields of a case class type -- only the ones declared in primary constructor */
    def caseFields(given ctx: Context): List[Symbol] =
      internal.Symbol_caseFields(self)

    def isTypeParam(given ctx: Context): Boolean =
      internal.Symbol_isTypeParam(self)

    /** Signature of this definition */
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
