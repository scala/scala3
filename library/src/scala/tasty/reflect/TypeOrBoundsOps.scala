package scala.tasty
package reflect

trait TypeOrBoundsOps extends Core {

  // ----- Types ----------------------------------------------------

  def typeOf[T: scala.quoted.Type]: Type

  implicit class TypeAPI(self: Type) {
    def =:=(that: Type)(implicit ctx: Context): Boolean = kernel.`Type_=:=`(self)(that)
    def <:<(that: Type)(implicit ctx: Context): Boolean = kernel.`Type_<:<`(self)(that)
    def widen(implicit ctx: Context): Type = kernel.Type_widen(self)
    def classSymbol(implicit ctx: Context): Option[ClassSymbol] = kernel.Type_classSymbol(self)
    def typeSymbol(implicit ctx: Context): Symbol = kernel.Type_typeSymbol(self)
    def isSingleton(implicit ctx: Context): Boolean = kernel.Type_isSingleton(self)
    def memberType(member: Symbol)(implicit ctx: Context): Type = kernel.Type_memberType(self)(member)
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

    val ConstantType: ConstantTypeModule
    abstract class ConstantTypeModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Constant]
    }

    val IsSymRef: IsSymRefModule
    abstract class IsSymRefModule {
      /** Matches any SymRef and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[SymRef]
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

    val TermRef: TermRefModule
    abstract class TermRefModule {
      def apply(qual: TypeOrBounds, name: String)(implicit ctx: Context): TermRef
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(String, TypeOrBounds /* Type | NoPrefix */)]
    }

    val IsTypeRef: IsTypeRefModule
    abstract class IsTypeRefModule {
      /** Matches any TypeRef and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[TypeRef]
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

    val SuperType: SuperTypeModule
    abstract class SuperTypeModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)]
    }

    val IsRefinement: IsRefinementModule
    abstract class IsRefinementModule {
      /** Matches any Refinement and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[Refinement]
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

    val AppliedType: AppliedTypeModule
    abstract class AppliedTypeModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, List[TypeOrBounds /* Type | TypeBounds */])]
    }

    val IsAnnotatedType: IsAnnotatedTypeModule
    abstract class IsAnnotatedTypeModule {
      /** Matches any AnnotatedType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[AnnotatedType]
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

    val AndType: AndTypeModule
    abstract class AndTypeModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)]
    }

    val IsOrType: IsOrTypeModule
    abstract class IsOrTypeModule {
      /** Matches any OrType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[OrType]
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

    val MatchType: MatchTypeModule
    abstract class MatchTypeModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type, List[Type])]
    }

    val IsByNameType: IsByNameTypeModule
    abstract class IsByNameTypeModule {
      /** Matches any ByNameType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[ByNameType]
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

    val ParamRef: ParamRefModule
    abstract class ParamRefModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(LambdaType[TypeOrBounds], Int)]
    }

    val IsThisType: IsThisTypeModule
    abstract class IsThisTypeModule {
      /** Matches any ThisType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[ThisType]
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

    val RecursiveThis: RecursiveThisModule
    abstract class RecursiveThisModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[RecursiveType]
    }

    val IsRecursiveType: IsRecursiveTypeModule
    abstract class IsRecursiveTypeModule {
      /** Matches any RecursiveType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[RecursiveType]
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

    val MethodType: MethodTypeModule
    abstract class MethodTypeModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(List[String], List[Type], Type)]
    }

    val IsPolyType: IsPolyTypeModule
    abstract class IsPolyTypeModule {
      /** Matches any PolyType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[PolyType]
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

    val TypeLambda: TypeLambdaModule
    abstract class TypeLambdaModule {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(List[String], List[TypeBounds], Type)]
    }

  }

  implicit class Type_ConstantTypeAPI(self: ConstantType) {
    def value(implicit ctx: Context): Any = kernel.ConstantType_value(self)
  }

  implicit class Type_SymRefAPI(self: SymRef) {
    def qualifier(implicit ctx: Context): TypeOrBounds /* Type | NoPrefix */ = kernel.SymRef_qualifier(self)
  }

  implicit class Type_TermRefAPI(self: TermRef) {
    def qualifier(implicit ctx: Context): TypeOrBounds /* Type | NoPrefix */ = kernel.TermRef_qualifier(self)
  }

  implicit class Type_TypeRefAPI(self: TypeRef) {
    def name(implicit ctx: Context): String = kernel.TypeRef_name(self)
    def qualifier(implicit ctx: Context): TypeOrBounds /* Type | NoPrefix */ = kernel.TypeRef_qualifier(self)
  }

  implicit class Type_SuperTypeAPI(self: SuperType) {
    def thistpe(implicit ctx: Context): Type = kernel.SuperType_thistpe(self)
    def supertpe(implicit ctx: Context): Type = kernel.SuperType_supertpe(self)
  }

  implicit class Type_RefinementAPI(self: Refinement) {
    def parent(implicit ctx: Context): Type = kernel.Refinement_parent(self)
    def name(implicit ctx: Context): String = kernel.Refinement_name(self)
    def info(implicit ctx: Context): TypeOrBounds = kernel.Refinement_info(self)
  }

  implicit class Type_AppliedTypeAPI(self: AppliedType) {
    def tycon(implicit ctx: Context): Type = kernel.AppliedType_tycon(self)
    def args(implicit ctx: Context): List[TypeOrBounds /* Type | TypeBounds */] = kernel.AppliedType_args(self)
  }

  implicit class Type_AnnotatedTypeAPI(self: AnnotatedType) {
    def underlying(implicit ctx: Context): Type = kernel.AnnotatedType_underlying(self)
    def annot(implicit ctx: Context): Term = kernel.AnnotatedType_annot(self)
  }

  implicit class Type_AndTypeAPI(self: AndType) {
    def left(implicit ctx: Context): Type = kernel.AndType_left(self)
    def right(implicit ctx: Context): Type = kernel.AndType_right(self)
  }

  implicit class Type_OrTypeAPI(self: OrType) {
    def left(implicit ctx: Context): Type = kernel.OrType_left(self)
    def right(implicit ctx: Context): Type = kernel.OrType_right(self)
  }

  implicit class Type_MatchTypeAPI(self: MatchType) {
    def bound(implicit ctx: Context): Type = kernel.MatchType_bound(self)
    def scrutinee(implicit ctx: Context): Type = kernel.MatchType_scrutinee(self)
    def cases(implicit ctx: Context): List[Type] = kernel.MatchType_cases(self)
  }

  implicit class Type_ByNameTypeAPI(self: ByNameType) {
    def underlying(implicit ctx: Context): Type = kernel.ByNameType_underlying(self)
  }

  implicit class Type_ParamRefAPI(self: ParamRef) {
    def binder(implicit ctx: Context): LambdaType[TypeOrBounds] = kernel.ParamRef_binder(self)
    def paramNum(implicit ctx: Context): Int = kernel.ParamRef_paramNum(self)
  }

  implicit class Type_ThisTypeAPI(self: ThisType) {
    def underlying(implicit ctx: Context): Type = kernel.ThisType_underlying(self)
  }

  implicit class Type_RecursiveThisAPI(self: RecursiveThis) {
    def binder(implicit ctx: Context): RecursiveType = kernel.RecursiveThis_binder(self)
  }

  implicit class Type_RecursiveTypeAPI(self: RecursiveType) {
    def underlying(implicit ctx: Context): Type = kernel.RecursiveType_underlying(self)
  }

  implicit class Type_MethodTypeAPI(self: MethodType) {
    def isImplicit: Boolean = kernel.MethodType_isImplicit(self)
    def isErased: Boolean = kernel.MethodType_isErased(self)
    def paramNames(implicit ctx: Context): List[String] = kernel.MethodType_paramNames(self)
    def paramTypes(implicit ctx: Context): List[Type] = kernel.MethodType_paramTypes(self)
    def resType(implicit ctx: Context): Type = kernel.MethodType_resType(self)
  }

  implicit class Type_PolyTypeAPI(self: PolyType) {
    def paramNames(implicit ctx: Context): List[String] = kernel.PolyType_paramNames(self)
    def paramBounds(implicit ctx: Context): List[TypeBounds] = kernel.PolyType_paramBounds(self)
    def resType(implicit ctx: Context): Type = kernel.PolyType_resType(self)
  }

  implicit class Type_TypeLambdaAPI(self: TypeLambda) {
    def paramNames(implicit ctx: Context): List[String] = kernel.TypeLambda_paramNames(self)
    def paramBounds(implicit ctx: Context): List[TypeBounds] = kernel.TypeLambda_paramBounds(self)
    def resType(implicit ctx: Context): Type = kernel.TypeLambda_resType(self)
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

  implicit class TypeBoundsAPI(self: TypeBounds) {
    def low(implicit ctx: Context): Type = kernel.TypeBounds_low(self)
    def hi(implicit ctx: Context): Type = kernel.TypeBounds_hi(self)
  }

  // ----- NoPrefix -------------------------------------------------

  val NoPrefix: NoPrefixModule
  abstract class NoPrefixModule {
    def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Boolean
  }

}
