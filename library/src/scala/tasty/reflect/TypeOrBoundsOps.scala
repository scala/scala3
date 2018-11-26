package scala.tasty
package reflect

trait TypeOrBoundsOps extends Core {

  // ----- Types ----------------------------------------------------

  trait TypeAPI {
    def =:=(other: Type)(implicit ctx: Context): Boolean
    def <:<(other: Type)(implicit ctx: Context): Boolean
  }
  implicit def TypeDeco(tpe: Type): TypeAPI

  trait MethodTypeAPI {
    def isImplicit: Boolean
    def isErased: Boolean
    def paramNames(implicit ctx: Context): List[String]
    def paramTypes(implicit ctx: Context): List[Type]
    def resultTpe(implicit ctx: Context): Type
  }
  implicit def MethodTypeDeco(tpt: MethodType): MethodTypeAPI

  trait PolyTypeAPI {
    def paramNames(implicit ctx: Context): List[String]
    def paramTypes(implicit ctx: Context): List[TypeBounds]
    def resultTpe(implicit ctx: Context): Type
  }
  implicit def PolyTypeDeco(tpt: PolyType): PolyTypeAPI

  trait TypeLambdaAPI {
    def paramNames(implicit ctx: Context): List[String]
    def paramTypes(implicit ctx: Context): List[TypeBounds]
    def resultTpe(implicit ctx: Context): Type
  }
  implicit def TypeLambdaDeco(tpt: TypeLambda): TypeLambdaAPI

  val IsType: IsTypeExtractor
  abstract class IsTypeExtractor {
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
    }
    implicit def ConstantTypeDeco(x: ConstantType): ConstantTypeAPI

    val ConstantType: ConstantTypeExtractor
    abstract class ConstantTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Constant]
    }

    val IsSymRef: IsSymRefModule
    abstract class IsSymRefModule {
      /** Matches any SymRef and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[SymRef]
    }

    trait SymRefAPI {
    }
    implicit def SymRefDeco(x: SymRef): SymRefAPI

    val SymRef: SymRefExtractor
    abstract class SymRefExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Symbol, TypeOrBounds /* Type | NoPrefix */)]
    }

    val IsTermRef: IsTermRefModule
    abstract class IsTermRefModule {
      /** Matches any TermRef and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[TermRef]
    }

    trait TermRefAPI {
    }
    implicit def TermRefDeco(x: TermRef): TermRefAPI

    val TermRef: TermRefExtractor
    abstract class TermRefExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(String, TypeOrBounds /* Type | NoPrefix */)]
    }

    val IsTypeRef: IsTypeRefModule
    abstract class IsTypeRefModule {
      /** Matches any TypeRef and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[TypeRef]
    }

    trait TypeRefAPI {
    }
    implicit def TypeRefDeco(x: TypeRef): TypeRefAPI

    val TypeRef: TypeRefExtractor
    abstract class TypeRefExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(String, TypeOrBounds /* Type | NoPrefix */)]
    }

    val IsSuperType: IsSuperTypeModule
    abstract class IsSuperTypeModule {
      /** Matches any SuperType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[SuperType]
    }

    trait SuperTypeAPI {
    }
    implicit def SuperTypeDeco(x: SuperType): SuperTypeAPI

    val SuperType: SuperTypeExtractor
    abstract class SuperTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)]
    }

    val IsRefinement: IsRefinementModule
    abstract class IsRefinementModule {
      /** Matches any Refinement and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[Refinement]
    }

    trait RefinementAPI {
    }
    implicit def RefinementDeco(x: Refinement): RefinementAPI

    val Refinement: RefinementExtractor
    abstract class RefinementExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, String, TypeOrBounds /* Type | TypeBounds */)]
    }

    val IsAppliedType: IsAppliedTypeModule
    abstract class IsAppliedTypeModule {
      /** Matches any AppliedType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[AppliedType]
    }

    trait AppliedTypeAPI {
    }
    implicit def AppliedTypeDeco(x: AppliedType): AppliedTypeAPI

    val AppliedType: AppliedTypeExtractor
    abstract class AppliedTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, List[TypeOrBounds /* Type | TypeBounds */])]
    }

    val IsAnnotatedType: IsAnnotatedTypeModule
    abstract class IsAnnotatedTypeModule {
      /** Matches any AnnotatedType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[AnnotatedType]
    }

    trait AnnotatedTypeAPI {
    }
    implicit def AnnotatedTypeDeco(x: AnnotatedType): AnnotatedTypeAPI

    val AnnotatedType: AnnotatedTypeExtractor
    abstract class AnnotatedTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Term)]
    }

    val IsAndType: IsAndTypeModule
    abstract class IsAndTypeModule {
      /** Matches any AndType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[AndType]
    }

    trait AndTypeAPI {
    }
    implicit def AndTypeDeco(x: AndType): AndTypeAPI

    val AndType: AndTypeExtractor
    abstract class AndTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)]
    }

    val IsOrType: IsOrTypeModule
    abstract class IsOrTypeModule {
      /** Matches any OrType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[OrType]
    }

    trait OrTypeAPI {
    }
    implicit def OrTypeDeco(x: OrType): OrTypeAPI

    val OrType: OrTypeExtractor
    abstract class OrTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)]
    }

    val IsMatchType: IsMatchTypeModule
    abstract class IsMatchTypeModule {
      /** Matches any MatchType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[MatchType]
    }

    trait MatchTypeAPI {
    }
    implicit def MatchTypeDeco(x: MatchType): MatchTypeAPI

    val MatchType: MatchTypeExtractor
    abstract class MatchTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type, List[Type])]
    }

    val IsByNameType: IsByNameTypeModule
    abstract class IsByNameTypeModule {
      /** Matches any ByNameType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[ByNameType]
    }

    trait ByNameTypeAPI {
    }
    implicit def ByNameTypeDeco(x: ByNameType): ByNameTypeAPI

    val ByNameType: ByNameTypeExtractor
    abstract class ByNameTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Type]
    }

    val IsParamRef: IsParamRefModule
    abstract class IsParamRefModule {
      /** Matches any ParamRef and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[ParamRef]
    }

    trait ParamRefAPI {
    }
    implicit def ParamRefDeco(x: ParamRef): ParamRefAPI

    val ParamRef: ParamRefExtractor
    abstract class ParamRefExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(LambdaType[TypeOrBounds], Int)]
    }

    val IsThisType: IsThisTypeModule
    abstract class IsThisTypeModule {
      /** Matches any ThisType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[ThisType]
    }

    trait ThisTypeAPI {
    }
    implicit def ThisTypeDeco(x: ThisType): ThisTypeAPI

    val ThisType: ThisTypeExtractor
    abstract class ThisTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Type]
    }

    val IsRecursiveThis: IsRecursiveThisModule
    abstract class IsRecursiveThisModule {
      /** Matches any RecursiveThis and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[RecursiveThis]
    }

    trait RecursiveThisAPI {
    }
    implicit def RecursiveThisDeco(x: RecursiveThis): RecursiveThisAPI

    val RecursiveThis: RecursiveThisExtractor
    abstract class RecursiveThisExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[RecursiveType]
    }

    val IsRecursiveType: IsRecursiveTypeModule
    abstract class IsRecursiveTypeModule {
      /** Matches any RecursiveType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[RecursiveType]
    }

    trait RecursiveTypeAPI {
    }
    implicit def RecursiveTypeDeco(x: RecursiveType): RecursiveTypeAPI

    val RecursiveType: RecursiveTypeExtractor
    abstract class RecursiveTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Type]
    }

    val IsMethodType: IsMethodTypeModule
    abstract class IsMethodTypeModule {
      /** Matches any MethodType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[MethodType]
    }

    trait MethodTypeAPI {
    }
    implicit def MethodTypeDeco(x: MethodType): MethodTypeAPI

    val MethodType: MethodTypeExtractor
    abstract class MethodTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(List[String], List[Type], Type)]
    }

    val IsPolyType: IsPolyTypeModule
    abstract class IsPolyTypeModule {
      /** Matches any PolyType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[PolyType]
    }

    trait PolyTypeAPI {
    }
    implicit def PolyTypeDeco(x: PolyType): PolyTypeAPI

    val PolyType: PolyTypeExtractor
    abstract class PolyTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(List[String], List[TypeBounds], Type)]
    }

    val IsTypeLambda: IsTypeLambdaModule
    abstract class IsTypeLambdaModule {
      /** Matches any TypeLambda and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[TypeLambda]
    }

    trait TypeLambdaAPI {
    }
    implicit def TypeLambdaDeco(x: TypeLambda): TypeLambdaAPI

    val TypeLambda: TypeLambdaExtractor
    abstract class TypeLambdaExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(List[String], List[TypeBounds], Type)]
    }

  }

  // ----- TypeBounds -----------------------------------------------

  val IsTypeBounds: IsTypeBoundsExtractor
  abstract class IsTypeBoundsExtractor {
    def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[TypeBounds]
  }

  val TypeBounds: TypeBoundsExtractor
  abstract class TypeBoundsExtractor {
    def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)]
  }

  trait TypeBoundsAPI {
    def low(implicit ctx: Context): Type
    def hi(implicit ctx: Context): Type
  }
  implicit def TypeBoundsDeco(bounds: TypeBounds): TypeBoundsAPI

  // ----- NoPrefix -------------------------------------------------

  val NoPrefix: NoPrefixExtractor
  abstract class NoPrefixExtractor {
    def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Boolean
  }

}
