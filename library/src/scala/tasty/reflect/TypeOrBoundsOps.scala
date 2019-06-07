package scala.tasty
package reflect

trait TypeOrBoundsOps extends Core {

  // ----- Types ----------------------------------------------------

  def typeOf[T: scala.quoted.Type]: Type

  implicit class TypeAPI(self: Type) {
    def =:=(that: Type)(implicit ctx: Context): Boolean = kernel.`Type_=:=`(self)(that)
    def <:<(that: Type)(implicit ctx: Context): Boolean = kernel.`Type_<:<`(self)(that)
    def widen(implicit ctx: Context): Type = kernel.Type_widen(self)

    /** Follow aliases and dereferences LazyRefs, annotated types and instantiated
     *  TypeVars until type is no longer alias type, annotated type, LazyRef,
     *  or instantiated type variable.
     */
    def dealias(implicit ctx: Context): Type = kernel.Type_dealias(self)

    def classSymbol(implicit ctx: Context): Option[ClassDefSymbol] = kernel.Type_classSymbol(self)
    def typeSymbol(implicit ctx: Context): Symbol = kernel.Type_typeSymbol(self)
    def isSingleton(implicit ctx: Context): Boolean = kernel.Type_isSingleton(self)
    def memberType(member: Symbol)(implicit ctx: Context): Type = kernel.Type_memberType(self)(member)

    /** Is this type an instance of a non-bottom subclass of the given class `cls`? */
    def derivesFrom(cls: ClassDefSymbol)(implicit ctx: Context): Boolean =
      kernel.Type_derivesFrom(self)(cls)

    /** Is this type a function type?
     *
     *  @return true if the dealised type of `self` without refinement is `FunctionN[T1, T2, ..., Tn]`
     *
     *  @note The function
     *
     *     - returns true for `given Int => Int` and `erased Int => Int`
     *     - returns false for `List[Int]`, despite that `List[Int] <:< Int => Int`.
     */
    def isFunctionType(implicit ctx: Context): Boolean = kernel.Type_isFunctionType(self)

    /** Is this type an implicit function type?
     *
     *  @see `isFunctionType`
     */
    def isImplicitFunctionType(implicit ctx: Context): Boolean = kernel.Type_isImplicitFunctionType(self)

    /** Is this type an erased function type?
     *
     *  @see `isFunctionType`
     */
    def isErasedFunctionType(implicit ctx: Context): Boolean = kernel.Type_isErasedFunctionType(self)

    /** Is this type a dependent function type?
     *
     *  @see `isFunctionType`
     */
    def isDependentFunctionType(implicit ctx: Context): Boolean = kernel.Type_isDependentFunctionType(self)
  }

  object IsType {
    def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Type] =
      kernel.matchType(typeOrBounds)
  }

  object Type {

    object IsConstantType {
      /** Matches any ConstantType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[ConstantType] =
        kernel.matchConstantType(tpe)
    }

    object ConstantType {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Constant] =
        kernel.matchConstantType(typeOrBounds).map(_.constant)
    }

    object IsSymRef {
      /** Matches any SymRef and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[SymRef] =
        kernel.matchSymRef(tpe)
    }

    object SymRef {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Symbol, TypeOrBounds /* Type | NoPrefix */)] =
        kernel.matchSymRef_unapply(typeOrBounds)
    }

    object IsTermRef {
      /** Matches any TermRef and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[TermRef] =
        kernel.matchTermRef(tpe)
    }

    object TermRef {
      // TODO should qual be a Type?
      def apply(qual: TypeOrBounds, name: String)(implicit ctx: Context): TermRef =
        kernel.TermRef_apply(qual, name)
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(String, TypeOrBounds /* Type | NoPrefix */)] =
        kernel.matchTermRef(typeOrBounds).map(x => (x.name, x.qualifier))
    }

    object IsTypeRef {
      /** Matches any TypeRef and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[TypeRef] =
        kernel.matchTypeRef(tpe)
    }

    object TypeRef {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(String, TypeOrBounds /* Type | NoPrefix */)] =
        kernel.matchTypeRef(typeOrBounds).map(x => (x.name, x.qualifier))
    }

    object IsSuperType {
      /** Matches any SuperType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[SuperType] =
        kernel.matchSuperType(tpe)
    }

    object SuperType {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)] =
        kernel.matchSuperType(typeOrBounds).map(x => (x.thistpe, x.supertpe))
    }

    object IsRefinement {
      /** Matches any Refinement and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[Refinement] =
        kernel.matchRefinement(tpe)
    }

    object Refinement {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, String, TypeOrBounds /* Type | TypeBounds */)] =
        kernel.matchRefinement(typeOrBounds).map(x => (x.parent, x.name, x.info))
    }

    object IsAppliedType {
      /** Matches any AppliedType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[AppliedType] =
        kernel.matchAppliedType(tpe)
    }

    object AppliedType {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, List[TypeOrBounds /* Type | TypeBounds */])] =
        kernel.matchAppliedType(typeOrBounds).map(x => (x.tycon, x.args))
    }

    object IsAnnotatedType {
      /** Matches any AnnotatedType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[AnnotatedType] =
        kernel.matchAnnotatedType(tpe)
    }

    object AnnotatedType {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Term)] =
        kernel.matchAnnotatedType(typeOrBounds).map(x => (x.underlying, x.annot))
    }

    object IsAndType {
      /** Matches any AndType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[AndType] =
        kernel.matchAndType(tpe)
    }

    object AndType {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)] =
        kernel.matchAndType(typeOrBounds).map(x => (x.left, x.right))
    }

    object IsOrType {
      /** Matches any OrType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[OrType] =
        kernel.matchOrType(tpe)
    }

    object OrType {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)] =
        kernel.matchOrType(typeOrBounds).map(x => (x.left, x.right))
    }

    object IsMatchType {
      /** Matches any MatchType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[MatchType] =
        kernel.matchMatchType(tpe)
    }

    object MatchType {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type, List[Type])] =
        kernel.matchMatchType(typeOrBounds).map(x => (x.bound, x.scrutinee, x.cases))
    }

    object IsByNameType {
      /** Matches any ByNameType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[ByNameType] =
        kernel.matchByNameType(tpe)
    }

    object ByNameType {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Type] =
        kernel.matchByNameType(typeOrBounds).map(_.underlying)
    }

    object IsParamRef {
      /** Matches any ParamRef and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[ParamRef] =
        kernel.matchParamRef(tpe)
    }

    object ParamRef {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(LambdaType[TypeOrBounds], Int)] =
        kernel.matchParamRef(typeOrBounds).map(x => (x.binder, x.paramNum))
    }

    object IsThisType {
      /** Matches any ThisType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[ThisType] =
        kernel.matchThisType(tpe)
    }

    object ThisType {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Type] =
        kernel.matchThisType(typeOrBounds).map(_.tref)
    }

    object IsRecursiveThis {
      /** Matches any RecursiveThis and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[RecursiveThis] =
        kernel.matchRecursiveThis(tpe)
    }

    object RecursiveThis {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[RecursiveType] =
        kernel.matchRecursiveThis(typeOrBounds).map(_.binder)
    }

    object IsRecursiveType {
      /** Matches any RecursiveType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[RecursiveType] =
        kernel.matchRecursiveType(tpe)
    }

    object RecursiveType {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Type] =
        kernel.matchRecursiveType(typeOrBounds).map(_.underlying)
    }

    object IsMethodType {
      /** Matches any MethodType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[MethodType] =
        kernel.matchMethodType(tpe)
    }

    object MethodType {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(List[String], List[Type], Type)] =
        kernel.matchMethodType(typeOrBounds).map(x => (x.paramNames, x.paramTypes, x.resType))
    }

    object IsPolyType {
      /** Matches any PolyType and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[PolyType] =
        kernel.matchPolyType(tpe)
    }

    object PolyType {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(List[String], List[TypeBounds], Type)] =
        kernel.matchPolyType(typeOrBounds).map(x => (x.paramNames, x.paramBounds, x.resType))
    }

    object IsTypeLambda {
      /** Matches any TypeLambda and returns it */
      def unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[TypeLambda] =
        kernel.matchTypeLambda(tpe)
    }

    object TypeLambda {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(List[String], List[TypeBounds], Type)] =
        kernel.matchTypeLambda(typeOrBounds).map(x => (x.paramNames, x.paramBounds, x.resType))
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
      kernel.matchTypeBounds(typeOrBounds)
  }

  object TypeBounds {
    def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)] =
      kernel.matchTypeBounds(typeOrBounds).map(x => (x.low, x.hi))
  }

  implicit class TypeBoundsAPI(self: TypeBounds) {
    def low(implicit ctx: Context): Type = kernel.TypeBounds_low(self)
    def hi(implicit ctx: Context): Type = kernel.TypeBounds_hi(self)
  }

  // ----- NoPrefix -------------------------------------------------

  object NoPrefix {
    def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Boolean =
      kernel.matchNoPrefix(typeOrBounds).isDefined
  }

}
