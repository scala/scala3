package scala.tasty
package reflect

trait StandardDefinitions extends Core {

  /** A value containing all standard definitions in [[DefinitionsAPI]]
    *  @group Definitions
    */
  object definitions extends StandardSymbols with StandardTypes

  /** Defines standard symbols (and types via its base trait).
    *  @group API
    */
  trait StandardSymbols {

    /** The module symbol of root package `_root_`. */
    def RootPackage: Symbol = kernel.Definitions_RootPackage

    /** The class symbol of root package `_root_`. */
    def RootClass: Symbol = kernel.Definitions_RootClass

    /** The class symbol of empty package `_root_._empty_`. */
    def EmptyPackageClass: Symbol = kernel.Definitions_EmptyPackageClass

    /** The module symbol of package `scala`. */
    def ScalaPackage: Symbol = kernel.Definitions_ScalaPackage

    /** The class symbol of package `scala`. */
    def ScalaPackageClass: Symbol = kernel.Definitions_ScalaPackageClass

    /** The class symbol of core class `scala.Any`. */
    def AnyClass: Symbol = kernel.Definitions_AnyClass

    /** The class symbol of core class `scala.AnyVal`. */
    def AnyValClass: Symbol = kernel.Definitions_AnyValClass

    /** The class symbol of core class `java.lang.Object`. */
    def ObjectClass: Symbol = kernel.Definitions_ObjectClass

    /** The type symbol of core class `scala.AnyRef`. */
    def AnyRefClass: Symbol = kernel.Definitions_AnyRefClass

    /** The class symbol of core class `scala.Null`. */
    def NullClass: Symbol = kernel.Definitions_NullClass

    /** The class symbol of core class `scala.Nothing`. */
    def NothingClass: Symbol = kernel.Definitions_NothingClass

    /** The class symbol of primitive class `scala.Unit`. */
    def UnitClass: Symbol = kernel.Definitions_UnitClass

    /** The class symbol of primitive class `scala.Byte`. */
    def ByteClass: Symbol = kernel.Definitions_ByteClass

    /** The class symbol of primitive class `scala.Short`. */
    def ShortClass: Symbol = kernel.Definitions_ShortClass

    /** The class symbol of primitive class `scala.Char`. */
    def CharClass: Symbol = kernel.Definitions_CharClass

    /** The class symbol of primitive class `scala.Int`. */
    def IntClass: Symbol = kernel.Definitions_IntClass

    /** The class symbol of primitive class `scala.Long`. */
    def LongClass: Symbol = kernel.Definitions_LongClass

    /** The class symbol of primitive class `scala.Float`. */
    def FloatClass: Symbol = kernel.Definitions_FloatClass

    /** The class symbol of primitive class `scala.Double`. */
    def DoubleClass: Symbol = kernel.Definitions_DoubleClass

    /** The class symbol of primitive class `scala.Boolean`. */
    def BooleanClass: Symbol = kernel.Definitions_BooleanClass

    /** The class symbol of class `scala.String`. */
    def StringClass: Symbol = kernel.Definitions_StringClass

    /** The class symbol of class `java.lang.Class`. */
    def ClassClass: Symbol = kernel.Definitions_ClassClass

    /** The class symbol of class `scala.Array`. */
    def ArrayClass: Symbol = kernel.Definitions_ArrayClass

    /** The module symbol of module `scala.Predef`. */
    def PredefModule: Symbol = kernel.Definitions_PredefModule

    /** The module symbol of package `java.lang`. */
    def JavaLangPackage: Symbol = kernel.Definitions_JavaLangPackage

    /** The module symbol of module `scala.Array`. */
    def ArrayModule: Symbol = kernel.Definitions_ArrayModule

    /** The method symbol of method `apply` in class `scala.Array`. */
    def Array_apply: Symbol = kernel.Definitions_Array_apply

    /** The method symbol of method `clone` in class `scala.Array`. */
    def Array_clone: Symbol = kernel.Definitions_Array_clone

    /** The method symbol of method `length` in class `scala.Array`. */
    def Array_length: Symbol = kernel.Definitions_Array_length

    /** The method symbol of method `update` in class `scala.Array`. */
    def Array_update: Symbol = kernel.Definitions_Array_update

    /** A dummy class symbol that is used to indicate repeated parameters
      *  compiled by the Scala compiler.
      */
    def RepeatedParamClass: ClassDefSymbol = kernel.Definitions_RepeatedParamClass

    /** The class symbol of class `scala.Option`. */
    def OptionClass: Symbol = kernel.Definitions_OptionClass

    /** The module symbol of module `scala.None`. */
    def NoneModule: Symbol = kernel.Definitions_NoneModule

    /** The module symbol of module `scala.Some`. */
    def SomeModule: Symbol = kernel.Definitions_SomeModule

    /** Function-like object that maps arity to symbols for classes `scala.Product` */
    def ProductClass: Symbol = kernel.Definitions_ProductClass

    /** Function-like object that maps arity to symbols for classes `scala.FunctionX`.
      *   -  0th element is `Function0`
      *   -  1st element is `Function1`
      *   -  ...
      *   -  Nth element is `FunctionN`
      */
    def FunctionClass(arity: Int, isImplicit: Boolean = false, isErased: Boolean = false): Symbol =
      kernel.Definitions_FunctionClass(arity, isImplicit, isErased)

    /** Function-like object that maps arity to symbols for classes `scala.TupleX`.
      *   -  0th element is `NoSymbol`
      *   -  1st element is `NoSymbol`
      *   -  2st element is `Tuple2`
      *   -  ...
      *   - 22nd element is `Tuple22`
      *   - 23nd element is `NoSymbol`  // TODO update when we will have more tuples
      *   - ...
      */
    def TupleClass(arity: Int): Symbol =
      kernel.Definitions_TupleClass(arity)

    /** Contains Scala primitive value classes:
      *   - Byte
      *   - Short
      *   - Int
      *   - Long
      *   - Float
      *   - Double
      *   - Char
      *   - Boolean
      *   - Unit
      */
    def ScalaPrimitiveValueClasses: List[Symbol] =
      UnitClass :: BooleanClass :: ScalaNumericValueClasses

    /** Contains Scala numeric value classes:
      *   - Byte
      *   - Short
      *   - Int
      *   - Long
      *   - Float
      *   - Double
      *   - Char
      */
    def ScalaNumericValueClasses: List[Symbol] =
      ByteClass :: ShortClass :: IntClass :: LongClass :: FloatClass :: DoubleClass :: CharClass :: Nil

  }

  /** Defines standard types.
    *  @group Definitions
    */
  trait StandardTypes {
    /** The type of primitive type `Unit`. */
    def UnitType: Type = kernel.Definitions_UnitType

    /** The type of primitive type `Byte`. */
    def ByteType: Type = kernel.Definitions_ByteType

    /** The type of primitive type `Short`. */
    def ShortType: Type = kernel.Definitions_ShortType

    /** The type of primitive type `Char`. */
    def CharType: Type = kernel.Definitions_CharType

    /** The type of primitive type `Int`. */
    def IntType: Type = kernel.Definitions_IntType

    /** The type of primitive type `Long`. */
    def LongType: Type = kernel.Definitions_LongType

    /** The type of primitive type `Float`. */
    def FloatType: Type = kernel.Definitions_FloatType

    /** The type of primitive type `Double`. */
    def DoubleType: Type = kernel.Definitions_DoubleType

    /** The type of primitive type `Boolean`. */
    def BooleanType: Type = kernel.Definitions_BooleanType

    /** The type of core type `Any`. */
    def AnyType: Type = kernel.Definitions_AnyType

    /** The type of core type `AnyVal`. */
    def AnyValType: Type = kernel.Definitions_AnyValType

    /** The type of core type `AnyRef`. */
    def AnyRefType: Type = kernel.Definitions_AnyRefType

    /** The type of core type `Object`. */
    def ObjectType: Type = kernel.Definitions_ObjectType

    /** The type of core type `Nothing`. */
    def NothingType: Type = kernel.Definitions_NothingType

    /** The type of core type `Null`. */
    def NullType: Type = kernel.Definitions_NullType

    /** The type for `scala.String`. */
    def StringType: Type = kernel.Definitions_StringType
  }
}
