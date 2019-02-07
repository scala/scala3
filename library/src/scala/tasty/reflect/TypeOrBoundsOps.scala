package scala.tasty
package reflect

trait TypeOrBoundsOps extends Core {

  implicit def TypeDeco(tpe: Type): TypeAPI

  implicit def ConstantTypeDeco(x: ConstantType): Type.ConstantTypeAPI

  implicit def SymRefDeco(x: SymRef): Type.SymRefAPI

  implicit def TermRefDeco(x: TermRef): Type.TermRefAPI

  implicit def TypeRefDeco(x: TypeRef): Type.TypeRefAPI

  implicit def SuperTypeDeco(x: SuperType): Type.SuperTypeAPI

  implicit def RefinementDeco(x: Refinement): Type.RefinementAPI

  implicit def AppliedTypeDeco(x: AppliedType): Type.AppliedTypeAPI

  implicit def AnnotatedTypeDeco(x: AnnotatedType): Type.AnnotatedTypeAPI

  implicit def AndTypeDeco(x: AndType): Type.AndTypeAPI

  implicit def OrTypeDeco(x: OrType): Type.OrTypeAPI

  implicit def MatchTypeDeco(x: MatchType): Type.MatchTypeAPI

  implicit def ByNameTypeDeco(x: ByNameType): Type.ByNameTypeAPI

  implicit def ParamRefDeco(x: ParamRef): Type.ParamRefAPI

  implicit def ThisTypeDeco(x: ThisType): Type.ThisTypeAPI

  implicit def RecursiveThisDeco(x: RecursiveThis): Type.RecursiveThisAPI

  implicit def RecursiveTypeDeco(x: RecursiveType): Type.RecursiveTypeAPI

  implicit def MethodTypeDeco(x: MethodType): Type.MethodTypeAPI

  implicit def PolyTypeDeco(x: PolyType): Type.PolyTypeAPI

  implicit def TypeLambdaDeco(x: TypeLambda): Type.TypeLambdaAPI

  implicit def TypeBoundsDeco(bounds: TypeBounds): TypeBoundsAPI

  // ----- Types ----------------------------------------------------

  def typeOf[T: scala.quoted.Type]: Type

  trait TypeAPI {
    def =:=(other: Type)(implicit ctx: Context): Boolean
    def <:<(other: Type)(implicit ctx: Context): Boolean
    def widen(implicit ctx: Context): Type
    def classSymbol(implicit ctx: Context): Option[ClassSymbol]
    def typeSymbol(implicit ctx: Context): Symbol
  }

  val IsType: IsTypeModule
  abstract class IsTypeModule {
    def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Type]
  }

  val Type: TypeModule
  abstract class TypeModule {

    val IsConstantType: IsConstantTypeModule
    abstract class IsConstantTypeModule {
      /** Matches any ConstantType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[ConstantType]
    }

    trait ConstantTypeAPI {
      def value(implicit ctx: Context): Any
    }

    val ConstantType: ConstantTypeModule
    abstract class ConstantTypeModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Constant]
    }

    val IsSymRef: IsSymRefModule
    abstract class IsSymRefModule {
      /** Matches any SymRef and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[SymRef]
    }

    trait SymRefAPI {
      def qualifier(implicit ctx: Context): TypeOrBounds /* Type | NoPrefix */
    }

    val SymRef: SymRefModule
    abstract class SymRefModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Symbol, TypeOrBounds /* Type | NoPrefix */)]
    }

    val IsTermRef: IsTermRefModule
    abstract class IsTermRefModule {
      /** Matches any TermRef and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[TermRef]
    }

    trait TermRefAPI {
      def qualifier(implicit ctx: Context): TypeOrBounds /* Type | NoPrefix */
    }

    val TermRef: TermRefModule
    abstract class TermRefModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(String, TypeOrBounds /* Type | NoPrefix */)]
    }

    val IsTypeRef: IsTypeRefModule
    abstract class IsTypeRefModule {
      /** Matches any TypeRef and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[TypeRef]
    }

    trait TypeRefAPI {
      def name(implicit ctx: Context): String
      def qualifier(implicit ctx: Context): TypeOrBounds /* Type | NoPrefix */
    }

    val TypeRef: TypeRefModule
    abstract class TypeRefModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(String, TypeOrBounds /* Type | NoPrefix */)]
    }

    val IsSuperType: IsSuperTypeModule
    abstract class IsSuperTypeModule {
      /** Matches any SuperType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[SuperType]
    }

    trait SuperTypeAPI {
      def thistpe(implicit ctx: Context): Type
      def supertpe(implicit ctx: Context): Type
    }

    val SuperType: SuperTypeModule
    abstract class SuperTypeModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)]
    }

    val IsRefinement: IsRefinementModule
    abstract class IsRefinementModule {
      /** Matches any Refinement and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[Refinement]
    }

    trait RefinementAPI {
      def parent(implicit ctx: Context): Type
      def name(implicit ctx: Context): String
      def info(implicit ctx: Context): TypeOrBounds
    }

    val Refinement: RefinementModule
    abstract class RefinementModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, String, TypeOrBounds /* Type | TypeBounds */)]
    }

    val IsAppliedType: IsAppliedTypeModule
    abstract class IsAppliedTypeModule {
      /** Matches any AppliedType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[AppliedType]
    }

    trait AppliedTypeAPI {
      def tycon(implicit ctx: Context): Type
      def args(implicit ctx: Context): List[TypeOrBounds /* Type | TypeBounds */]
    }

    val AppliedType: AppliedTypeModule
    abstract class AppliedTypeModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, List[TypeOrBounds /* Type | TypeBounds */])]
    }

    val IsAnnotatedType: IsAnnotatedTypeModule
    abstract class IsAnnotatedTypeModule {
      /** Matches any AnnotatedType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[AnnotatedType]
    }

    trait AnnotatedTypeAPI {
      def underlying(implicit ctx: Context): Type
      def annot(implicit ctx: Context): Term
    }

    val AnnotatedType: AnnotatedTypeModule
    abstract class AnnotatedTypeModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Term)]
    }

    val IsAndType: IsAndTypeModule
    abstract class IsAndTypeModule {
      /** Matches any AndType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[AndType]
    }

    trait AndTypeAPI {
      def left(implicit ctx: Context): Type
      def right(implicit ctx: Context): Type
    }

    val AndType: AndTypeModule
    abstract class AndTypeModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)]
    }

    val IsOrType: IsOrTypeModule
    abstract class IsOrTypeModule {
      /** Matches any OrType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[OrType]
    }

    trait OrTypeAPI {
      def left(implicit ctx: Context): Type
      def right(implicit ctx: Context): Type
    }

    val OrType: OrTypeModule
    abstract class OrTypeModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)]
    }

    val IsMatchType: IsMatchTypeModule
    abstract class IsMatchTypeModule {
      /** Matches any MatchType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[MatchType]
    }

    trait MatchTypeAPI {
      def bound(implicit ctx: Context): Type
      def scrutinee(implicit ctx: Context): Type
      def cases(implicit ctx: Context): List[Type]
    }

    val MatchType: MatchTypeModule
    abstract class MatchTypeModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type, List[Type])]
    }

    val IsByNameType: IsByNameTypeModule
    abstract class IsByNameTypeModule {
      /** Matches any ByNameType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[ByNameType]
    }

    trait ByNameTypeAPI {
      def underlying(implicit ctx: Context): Type
    }

    val ByNameType: ByNameTypeModule
    abstract class ByNameTypeModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Type]
    }

    val IsParamRef: IsParamRefModule
    abstract class IsParamRefModule {
      /** Matches any ParamRef and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[ParamRef]
    }

    trait ParamRefAPI {
      def binder(implicit ctx: Context): LambdaType[TypeOrBounds]
      def paramNum(implicit ctx: Context): Int
    }

    val ParamRef: ParamRefModule
    abstract class ParamRefModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(LambdaType[TypeOrBounds], Int)]
    }

    val IsThisType: IsThisTypeModule
    abstract class IsThisTypeModule {
      /** Matches any ThisType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[ThisType]
    }

    trait ThisTypeAPI {
      def underlying(implicit ctx: Context): Type
    }

    val ThisType: ThisTypeModule
    abstract class ThisTypeModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Type]
    }

    val IsRecursiveThis: IsRecursiveThisModule
    abstract class IsRecursiveThisModule {
      /** Matches any RecursiveThis and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[RecursiveThis]
    }

    trait RecursiveThisAPI {
      def binder(implicit ctx: Context): RecursiveType
    }

    val RecursiveThis: RecursiveThisModule
    abstract class RecursiveThisModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[RecursiveType]
    }

    val IsRecursiveType: IsRecursiveTypeModule
    abstract class IsRecursiveTypeModule {
      /** Matches any RecursiveType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[RecursiveType]
    }

    trait RecursiveTypeAPI {
      def underlying(implicit ctx: Context): Type
    }

    val RecursiveType: RecursiveTypeModule
    abstract class RecursiveTypeModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Type]
    }

    val IsMethodType: IsMethodTypeModule
    abstract class IsMethodTypeModule {
      /** Matches any MethodType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[MethodType]
    }

    trait MethodTypeAPI {
      def isImplicit: Boolean
      def isErased: Boolean
      def paramNames(implicit ctx: Context): List[String]
      def paramTypes(implicit ctx: Context): List[Type]
      def resType(implicit ctx: Context): Type
    }

    val MethodType: MethodTypeModule
    abstract class MethodTypeModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(List[String], List[Type], Type)]
    }

    val IsPolyType: IsPolyTypeModule
    abstract class IsPolyTypeModule {
      /** Matches any PolyType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[PolyType]
    }

    trait PolyTypeAPI {
      def paramNames(implicit ctx: Context): List[String]
      def paramBounds(implicit ctx: Context): List[TypeBounds]
      def resType(implicit ctx: Context): Type
    }

    val PolyType: PolyTypeModule
    abstract class PolyTypeModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(List[String], List[TypeBounds], Type)]
    }

    val IsTypeLambda: IsTypeLambdaModule
    abstract class IsTypeLambdaModule {
      /** Matches any TypeLambda and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[TypeLambda]
    }

    trait TypeLambdaAPI {
      def paramNames(implicit ctx: Context): List[String]
      def paramBounds(implicit ctx: Context): List[TypeBounds]
      def resType(implicit ctx: Context): Type
    }

    val TypeLambda: TypeLambdaModule
    abstract class TypeLambdaModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(List[String], List[TypeBounds], Type)]
    }

  }

  // ----- TypeBounds -----------------------------------------------

  val IsTypeBounds: IsTypeBoundsModule
  abstract class IsTypeBoundsModule {
    def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[TypeBounds]
  }

  val TypeBounds: TypeBoundsModule
  abstract class TypeBoundsModule {
    def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)]
  }

  trait TypeBoundsAPI {
    def low(implicit ctx: Context): Type
    def hi(implicit ctx: Context): Type
  }

  // ----- NoPrefix -------------------------------------------------

  val NoPrefix: NoPrefixModule
  abstract class NoPrefixModule {
    def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Boolean
  }

}
