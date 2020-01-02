package dotty.tools.tasty.experimental.bridge

import reflect.ClassTag

trait SymbolOps extends Core with

  given ClassTag[Symbol] = internal.Symbol_CT
  given ClassTag[ClassSymbol] = internal.ClassSymbol_CT

  object defn with
    def throwMethod(given Context): TermSymbol = internal.defn_throwMethod
    def BodyAnnot(given Context): ClassSymbol = internal.defn_BodyAnnot

  object Symbols with

    type MutableSymbolMap[T] = internal.Symbols_MutableSymbolMap[T]

    def newMutableSymbolMap[T]: MutableSymbolMap[T] = internal.Symbols_newMutableSymbolMap[T]

    given MutableSymbolMapOps: [T](map: Symbols.MutableSymbolMap[T]) extended with
      def get(sym: Symbol): Option[T] = internal.Symbols_MutableSymbolMap_get(map, sym)
      def contains(sym: Symbol): Boolean = internal.Symbols_MutableSymbolMap_contains(map, sym)
      def update(sym: Symbol, value: T): Unit = internal.Symbols_MutableSymbolMap_update(map, sym, value)
      def -=(sym: Symbol): Unit = internal.Symbols_MutableSymbolMap_-=(map, sym)
      def apply(sym: Symbol): T = internal.Symbols_MutableSymbolMap_apply(map, sym)
      def isEmpty: Boolean = internal.Symbols_MutableSymbolMap_isEmpty(map)
      def keysIterator: Iterator[Symbol] = internal.Symbols_MutableSymbolMap_keysIterator(map)
    end MutableSymbolMapOps

    given MutableSymbolMapOpsPlus: AnyRef with
      def [T, U >: T](map: Symbols.MutableSymbolMap[T]) getOrElse(sym: Symbol, default: => U): U =
        internal.Symbols_MutableSymbolMap_getOrElse(map, sym, default)
    end MutableSymbolMapOpsPlus

  end Symbols

  given SymbolOps: (sym: Symbol) extended with
    def isPackage(given Context): Boolean = internal.Symbol_isPackage(sym)
    def isPrivate(given Context): Boolean = internal.Symbol_isPrivate(sym)
    def sourcePos(given Context): SourcePosition = internal.Symbol_sourcePos(sym)
    def owner(given Context): Symbol = internal.Symbol_owner(sym)
    def isDefinedWithin(outer: Symbol)(given Context): Boolean = internal.Symbol_isDefinedWithin(sym, outer)
    def termRef(given Context): TermRef = internal.Symbol_termRef(sym)
    def typeRef(given Context): TypeRef = internal.Symbol_typeRef(sym)
    def name(given Context): sym.ThisName = internal.Symbol_name(sym)
    def fullName(given Context): Name = internal.Symbol_fullName(sym)
    def isClass: Boolean = internal.Symbol_isClass(sym)
    def isEffectiveRoot(given Context): Boolean = internal.Symbol_isEffectiveRoot(sym)
    def flags(given Context): FlagSet = internal.Symbol_flags(sym)
    def privateWithin(given Context): Symbol = internal.Symbol_privateWithin(sym)
    def isTerm(given Context): Boolean = internal.Symbol_isTerm(sym)
    def isSetter(given Context): Boolean = internal.Symbol_isSetter(sym)
    def info(given Context): Type = internal.Symbol_info(sym)
    def isInaccessibleChildOf(cls: Symbol)(given Context): Boolean = internal.Symbol_isInaccessibleChildOf(sym, cls)
    def exists(given Context): Boolean = internal.Symbol_exists(sym)
    def showLocated(given Context): String = internal.Symbol_showLocated(sym)
    def annotations(given Context): List[Annotation] = internal.Symbol_annotations(sym)
    def asClass: ClassSymbol = sym.asInstanceOf[ClassSymbol]
  end SymbolOps
