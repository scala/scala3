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

  object IsType {
    def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Type] =
      kernel.isType(typeOrBounds)
  }

  object Type {

    object IsConstantType {
      /** Matches any ConstantType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[ConstantType] =
        kernel.isConstantType(tpe)
    }

    object ConstantType {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Constant] =
        kernel.isConstantType(typeOrBounds).map(_.constant)
    }

    object IsSymRef {
      /** Matches any SymRef and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[SymRef] =
        kernel.isSymRef(tpe)
    }

    object SymRef {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Symbol, TypeOrBounds /* Type | NoPrefix */)] =
        kernel.isSymRef_unapply(typeOrBounds)
    }

    object IsTermRef {
      /** Matches any TermRef and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[TermRef] =
        kernel.isTermRef(tpe)
    }

    object TermRef {
      // TODO should qual be a Type?
      def apply(qual: TypeOrBounds, name: String)(implicit ctx: Context): TermRef =
        kernel.TermRef_apply(qual, name)
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(String, TypeOrBounds /* Type | NoPrefix */)] =
        kernel.isTermRef(typeOrBounds).map(x => (x.name, x.qualifier))
    }

    object IsTypeRef {
      /** Matches any TypeRef and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[TypeRef] =
        kernel.isTypeRef(tpe)
    }

    object TypeRef {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(String, TypeOrBounds /* Type | NoPrefix */)] =
        kernel.isTypeRef(typeOrBounds).map(x => (x.name, x.qualifier))
    }

    object IsSuperType {
      /** Matches any SuperType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[SuperType] =
        kernel.isSuperType(tpe)
    }

    object SuperType {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)] =
        kernel.isSuperType(typeOrBounds).map(x => (x.thistpe, x.supertpe))
    }

    object IsRefinement {
      /** Matches any Refinement and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[Refinement] =
        kernel.isRefinement(tpe)
    }

    object Refinement {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, String, TypeOrBounds /* Type | TypeBounds */)] =
        kernel.isRefinement(typeOrBounds).map(x => (x.parent, x.name, x.info))
    }

    object IsAppliedType {
      /** Matches any AppliedType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[AppliedType] =
        kernel.isAppliedType(tpe)
    }

    object AppliedType {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, List[TypeOrBounds /* Type | TypeBounds */])] =
        kernel.isAppliedType(typeOrBounds).map(x => (x.tycon, x.args))
    }

    object IsAnnotatedType {
      /** Matches any AnnotatedType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[AnnotatedType] =
        kernel.isAnnotatedType(tpe)
    }

    object AnnotatedType {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Term)] =
        kernel.isAnnotatedType(typeOrBounds).map(x => (x.underlying, x.annot))
    }

    object IsAndType {
      /** Matches any AndType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[AndType] =
        kernel.isAndType(tpe)
    }

    object AndType {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)] =
        kernel.isAndType(typeOrBounds).map(x => (x.left, x.right))
    }

    object IsOrType {
      /** Matches any OrType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[OrType] =
        kernel.isOrType(tpe)
    }

    object OrType {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)] =
        kernel.isOrType(typeOrBounds).map(x => (x.left, x.right))
    }

    object IsMatchType {
      /** Matches any MatchType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[MatchType] =
        kernel.isMatchType(tpe)
    }

    object MatchType {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type, List[Type])] =
        kernel.isMatchType(typeOrBounds).map(x => (x.bound, x.scrutinee, x.cases))
    }

    object IsByNameType {
      /** Matches any ByNameType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[ByNameType] =
        kernel.isByNameType(tpe)
    }

    object ByNameType {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Type] =
        kernel.isByNameType(typeOrBounds).map(_.underlying)
    }

    object IsParamRef {
      /** Matches any ParamRef and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[ParamRef] =
        kernel.isParamRef(tpe)
    }

    object ParamRef {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(LambdaType[TypeOrBounds], Int)] =
        kernel.isParamRef(typeOrBounds).map(x => (x.binder, x.paramNum))
    }

    object IsThisType {
      /** Matches any ThisType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[ThisType] =
        kernel.isThisType(tpe)
    }

    object ThisType {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Type] =
        kernel.isThisType(typeOrBounds).map(_.tref)
    }

    object IsRecursiveThis {
      /** Matches any RecursiveThis and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[RecursiveThis] =
        kernel.isRecursiveThis(tpe)
    }

    object RecursiveThis {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[RecursiveType] =
        kernel.isRecursiveThis(typeOrBounds).map(_.binder)
    }

    object IsRecursiveType {
      /** Matches any RecursiveType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[RecursiveType] =
        kernel.isRecursiveType(tpe)
    }

    object RecursiveType {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Type] =
        kernel.isRecursiveType(typeOrBounds).map(_.underlying)
    }

    object IsMethodType {
      /** Matches any MethodType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[MethodType] =
        kernel.isMethodType(tpe)
    }

    object MethodType {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(List[String], List[Type], Type)] =
        kernel.isMethodType(typeOrBounds).map(x => (x.paramNames, x.paramTypes, x.resType))
    }

    object IsPolyType {
      /** Matches any PolyType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[PolyType] =
        kernel.isPolyType(tpe)
    }

    object PolyType {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(List[String], List[TypeBounds], Type)] =
        kernel.isPolyType(typeOrBounds).map(x => (x.paramNames, x.paramBounds, x.resType))
    }

    object IsTypeLambda {
      /** Matches any TypeLambda and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[TypeLambda] =
        kernel.isTypeLambda(tpe)
    }

    object TypeLambda {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(List[String], List[TypeBounds], Type)] =
        kernel.isTypeLambda(typeOrBounds).map(x => (x.paramNames, x.paramBounds, x.resType))
    }

  }

  implicit class Type_ConstantTypeAPI(self: ConstantType) {
    def constant(implicit ctx: Context): Constant = kernel.ConstantType_constant(self)
  }

  implicit class Type_SymRefAPI(self: SymRef) {
    def qualifier(implicit ctx: Context): TypeOrBounds /* Type | NoPrefix */ = kernel.SymRef_qualifier(self)
  }

  implicit class Type_TermRefAPI(self: TermRef) {
    def name(implicit ctx: Context): String = kernel.TermRef_name(self)
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
    def tref(implicit ctx: Context): Type = kernel.ThisType_tref(self)
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

  object IsTypeBounds {
    def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[TypeBounds] =
      kernel.isTypeBounds(typeOrBounds)
  }

  object TypeBounds {
    def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)] =
      kernel.isTypeBounds(typeOrBounds).map(x => (x.low, x.hi))
  }

  implicit class TypeBoundsAPI(self: TypeBounds) {
    def low(implicit ctx: Context): Type = kernel.TypeBounds_low(self)
    def hi(implicit ctx: Context): Type = kernel.TypeBounds_hi(self)
  }

  // ----- NoPrefix -------------------------------------------------

  object NoPrefix {
    def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Boolean =
      kernel.isNoPrefix(typeOrBounds).isDefined
  }

}
