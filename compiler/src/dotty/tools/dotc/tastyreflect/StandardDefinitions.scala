package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.core.Symbols._

import dotty.tools.dotc.tastyreflect.FromSymbol._

trait StandardDefinitions extends scala.tasty.reflect.StandardDefinitions {
    tasty: TastyImpl =>

  private implicit def ctx: Context = rootContext

  val definitions: DefinitionsApi = new DefinitionsApi {

    def RootPackage: Symbol = defn.RootPackage
    def RootClass: Symbol = defn.RootClass

    def EmptyPackageClass: Symbol = defn.EmptyPackageClass

    def ScalaPackage: Symbol = defn.ScalaPackageVal
    def ScalaPackageClass: Symbol = defn.ScalaPackageClass

    def AnyClass: Symbol = defn.AnyClass
    def AnyValClass: Symbol = defn.AnyValClass
    def ObjectClass: Symbol = defn.ObjectClass
    def AnyRefClass: Symbol = defn.AnyRefAlias
    def NullClass: Symbol = defn.AnyClass
    def NothingClass: Symbol = defn.NothingClass
    def UnitClass: Symbol = defn.UnitClass
    def ByteClass: Symbol = defn.ByteClass
    def ShortClass: Symbol = defn.ShortClass
    def CharClass: Symbol = defn.CharClass
    def IntClass: Symbol = defn.IntClass
    def LongClass: Symbol = defn.LongClass
    def FloatClass: Symbol = defn.FloatClass
    def DoubleClass: Symbol = defn.DoubleClass
    def BooleanClass: Symbol = defn.BooleanClass
    def StringClass: Symbol = defn.StringClass
    def ClassClass: Symbol = defn.ClassClass
    def ArrayClass: Symbol = defn.ArrayClass
    def PredefModule: Symbol = defn.ScalaPredefModule.asTerm

    def JavaLangPackage: Symbol = defn.JavaLangPackageVal

    def ArrayModule: Symbol = defn.ArrayClass.companionModule.asTerm

    def Array_apply: Symbol = defn.Array_apply.asTerm
    def Array_clone: Symbol = defn.Array_clone.asTerm
    def Array_length: Symbol = defn.Array_length.asTerm
    def Array_update: Symbol = defn.Array_update.asTerm

    def RepeatedParamClass: Symbol = defn.RepeatedParamClass

    def OptionClass: Symbol = defn.OptionClass
    def NoneModule: Symbol = defn.NoneClass.companionModule.asTerm
    def SomeModule: Symbol = defn.SomeClass.companionModule.asTerm

    def ProductClass: Symbol = defn.ProductClass
    def FunctionClass(arity: Int, isImplicit: Boolean = false, isErased: Boolean = false): Symbol =
      defn.FunctionClass(arity, isImplicit, isErased).asClass
    def TupleClass(arity: Int): Symbol = defn.TupleType(arity).classSymbol.asClass


    def ScalaPrimitiveValueClasses: List[Symbol] =
      UnitClass :: BooleanClass :: ScalaNumericValueClasses
    def ScalaNumericValueClasses: List[Symbol] =
      ByteClass :: ShortClass :: IntClass :: LongClass :: FloatClass :: DoubleClass :: CharClass :: Nil

    def UnitType: Type = defn.UnitType
    def ByteType: Type = defn.ByteType
    def ShortType: Type = defn.ShortType
    def CharType: Type = defn.CharType
    def IntType: Type = defn.IntType
    def LongType: Type = defn.LongType
    def FloatType: Type = defn.FloatType
    def DoubleType: Type = defn.DoubleType
    def BooleanType: Type = defn.BooleanType
    def AnyType: Type = defn.AnyType
    def AnyValType: Type = defn.AnyValType
    def AnyRefType: Type = defn.AnyRefType
    def ObjectType: Type = defn.ObjectType
    def NothingType: Type = defn.NothingType
    def NullType: Type = defn.NullType
  }
}
