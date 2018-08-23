package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.core.Symbols._

import dotty.tools.dotc.tastyreflect.FromSymbol._

trait StandardDefinitions extends scala.tasty.reflect.StandardDefinitions {
    tasty: TastyImpl =>

  private implicit def ctx: Context = rootContext

  val definitions: DefinitionsApi = new DefinitionsApi {

    // TODO return Symbols instead of Definitions

    def RootPackage: PackageDef = packageDefFromSym(defn.RootPackage)
    def RootClass: Symbol = defn.RootClass

    def EmptyPackageClass: Symbol = defn.EmptyPackageClass

    def ScalaPackage: Symbol = defn.ScalaPackageVal
    def ScalaPackageClass: Symbol = defn.ScalaPackageClass

    def AnyClass: ClassDef = classDef(defn.AnyClass)
    def AnyValClass: ClassDef = classDef(defn.AnyValClass)
    def ObjectClass: ClassDef = classDef(defn.ObjectClass)
    def AnyRefClass: TypeDef = typeDefFromSym(defn.AnyRefAlias)
    def NullClass: ClassDef = classDef(defn.AnyClass)
    def NothingClass: ClassDef = classDef(defn.NothingClass)
    def UnitClass: ClassDef = classDef(defn.UnitClass)
    def ByteClass: ClassDef = classDef(defn.ByteClass)
    def ShortClass: ClassDef = classDef(defn.ShortClass)
    def CharClass: ClassDef = classDef(defn.CharClass)
    def IntClass: ClassDef = classDef(defn.IntClass)
    def LongClass: ClassDef = classDef(defn.LongClass)
    def FloatClass: ClassDef = classDef(defn.FloatClass)
    def DoubleClass: ClassDef = classDef(defn.DoubleClass)
    def BooleanClass: ClassDef = classDef(defn.BooleanClass)
    def StringClass: ClassDef = classDef(defn.StringClass)
    def ClassClass: ClassDef = classDef(defn.ClassClass)
    def ArrayClass: ClassDef = classDef(defn.ArrayClass)
    def PredefModule: ValDef = valDefFromSym(defn.ScalaPredefModule.asTerm)

    def JavaLangPackage: PackageDef = packageDefFromSym(defn.JavaLangPackageVal)

    def ArrayModule: ValDef = valDefFromSym(defn.ArrayClass.companionModule.asTerm)

    def Array_apply: DefDef = defDefFromSym(defn.Array_apply.asTerm)
    def Array_clone: DefDef = defDefFromSym(defn.Array_clone.asTerm)
    def Array_length: DefDef = defDefFromSym(defn.Array_length.asTerm)
    def Array_update: DefDef = defDefFromSym(defn.Array_update.asTerm)

    def RepeatedParamClass: Symbol = defn.RepeatedParamClass

    def OptionClass: TypeDef = classDef(defn.OptionClass)
    def NoneModule: ValDef = valDefFromSym(defn.NoneClass.companionModule.asTerm)
    def SomeModule: ValDef = valDefFromSym(defn.SomeClass.companionModule.asTerm)

    def ProductClass: ClassDef = classDef(defn.ProductClass)
    def FunctionClass(arity: Int, isImplicit: Boolean = false, isErased: Boolean = false): ClassDef =
      classDef(defn.FunctionClass(arity, isImplicit, isErased).asClass)
    def TupleClass(arity: Int): ClassDef = classDef(defn.TupleType(arity).classSymbol.asClass)


    def ScalaPrimitiveValueClasses: List[ClassDef] =
      UnitClass :: BooleanClass :: ScalaNumericValueClasses
    def ScalaNumericValueClasses: List[ClassDef] =
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
