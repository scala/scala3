package scala.tasty
package reflect

trait StandardDefinitions extends TastyCore {

  /** A value containing all standard definitions in [[DefinitionsApi]]
    *  @group Definitions
    */
  val definitions: DefinitionsApi

  /** Defines standard symbols (and types via its base trait).
    *  @group API
    */
  trait DefinitionsApi extends StandardTypes {

    /** The module symbol of root package `_root_`. */
    def RootPackage: PackageDef

    /** The class symbol of root package `_root_`. */
    def RootClass: Symbol

    /** The class symbol of empty package `_root_._empty_`. */
    def EmptyPackageClass: Symbol

    /** The module symbol of package `scala`. */
    def ScalaPackage: Symbol

    /** The class symbol of package `scala`. */
    def ScalaPackageClass: Symbol

    /** The class symbol of core class `scala.Any`. */
    def AnyClass   : ClassDef

    /** The class symbol of core class `scala.AnyVal`. */
    def AnyValClass: ClassDef

    /** The class symbol of core class `java.lang.Object`. */
    def ObjectClass: ClassDef

    /** The type symbol of core class `scala.AnyRef`. */
    def AnyRefClass: TypeDef

    /** The class symbol of core class `scala.Null`. */
    def NullClass   : ClassDef

    /** The class symbol of core class `scala.Nothing`. */
    def NothingClass: ClassDef

    /** The class symbol of primitive class `scala.Unit`. */
    def UnitClass   : ClassDef

    /** The class symbol of primitive class `scala.Byte`. */
    def ByteClass   : ClassDef

    /** The class symbol of primitive class `scala.Short`. */
    def ShortClass  : ClassDef

    /** The class symbol of primitive class `scala.Char`. */
    def CharClass   : ClassDef

    /** The class symbol of primitive class `scala.Int`. */
    def IntClass    : ClassDef

    /** The class symbol of primitive class `scala.Long`. */
    def LongClass   : ClassDef

    /** The class symbol of primitive class `scala.Float`. */
    def FloatClass  : ClassDef

    /** The class symbol of primitive class `scala.Double`. */
    def DoubleClass : ClassDef

    /** The class symbol of primitive class `scala.Boolean`. */
    def BooleanClass: ClassDef

    /** The class symbol of class `scala.String`. */
    def StringClass : ClassDef

    /** The class symbol of class `java.lang.Class`. */
    def ClassClass  : ClassDef

    /** The class symbol of class `scala.Array`. */
    def ArrayClass  : ClassDef

    /** The module symbol of module `scala.Predef`. */
    def PredefModule: ValDef

    /** The module symbol of package `java.lang`. */
    def JavaLangPackage: PackageDef

    /** The module symbol of module `scala.Array`. */
    def ArrayModule: ValDef

    /** The method symbol of method `apply` in class `scala.Array`. */
    def Array_apply: DefDef

    /** The method symbol of method `clone` in class `scala.Array`. */
    def Array_clone: DefDef

    /** The method symbol of method `length` in class `scala.Array`. */
    def Array_length: DefDef

    /** The method symbol of method `update` in class `scala.Array`. */
    def Array_update: DefDef

    /** A dummy class symbol that is used to indicate repeated parameters
      *  compiled by the Scala compiler.
      */
    def RepeatedParamClass: Symbol

    /** The class symbol of class `scala.Option`. */
    def OptionClass: ClassDef

    /** The module symbol of module `scala.None`. */
    def NoneModule: ValDef

    /** The module symbol of module `scala.Some`. */
    def SomeModule: ValDef

    /** Function-like object that maps arity to symbols for classes `scala.Product` */
    def ProductClass: ClassDef

    /** Function-like object that maps arity to symbols for classes `scala.FunctionX`.
      *   -  0th element is `Function0`
      *   -  1st element is `Function1`
      *   -  ...
      *   -  Nth element is `FunctionN`
      */
    def FunctionClass(arity: Int, isImplicit: Boolean = false, isErased: Boolean = false): ClassDef

    /** Function-like object that maps arity to symbols for classes `scala.TupleX`.
      *   -  0th element is `NoSymbol`
      *   -  1st element is `NoSymbol`
      *   -  2st element is `Tuple2`
      *   -  ...
      *   - 22nd element is `Tuple22`
      *   - 23nd element is `NoSymbol`  // TODO update when we will have more tuples
      *   - ...
      */
    def TupleClass(arity: Int): ClassDef

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
    def ScalaPrimitiveValueClasses: List[ClassDef]

    /** Contains Scala numeric value classes:
      *   - Byte
      *   - Short
      *   - Int
      *   - Long
      *   - Float
      *   - Double
      *   - Char
      */
    def ScalaNumericValueClasses: List[ClassDef]
  }

  /** Defines standard types.
    *  @group Definitions
    */
  trait StandardTypes {
    /** The type of primitive type `Unit`. */
    def UnitType: Type

    /** The type of primitive type `Byte`. */
    def ByteType: Type

    /** The type of primitive type `Short`. */
    def ShortType: Type

    /** The type of primitive type `Char`. */
    def CharType: Type

    /** The type of primitive type `Int`. */
    def IntType: Type

    /** The type of primitive type `Long`. */
    def LongType: Type

    /** The type of primitive type `Float`. */
    def FloatType: Type

    /** The type of primitive type `Double`. */
    def DoubleType: Type

    /** The type of primitive type `Boolean`. */
    def BooleanType: Type

    /** The type of core type `Any`. */
    def AnyType: Type

    /** The type of core type `AnyVal`. */
    def AnyValType: Type

    /** The type of core type `AnyRef`. */
    def AnyRefType: Type

    /** The type of core type `Object`. */
    def ObjectType: Type

    /** The type of core type `Nothing`. */
    def NothingType: Type

    /** The type of core type `Null`. */
    def NullType: Type
  }
}
