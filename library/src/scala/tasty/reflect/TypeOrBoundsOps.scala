package scala.tasty
package reflect

trait TypeOrBoundsOps extends Core {

  // ----- Types ----------------------------------------------------

  def typeOf[T: scala.quoted.Type]: Type

  given (self: Type) {
    def =:=(that: Type)(given ctx: Context): Boolean = internal.`Type_=:=`(self)(that)
    def <:<(that: Type)(given ctx: Context): Boolean = internal.`Type_<:<`(self)(that)
    def widen(given ctx: Context): Type = internal.Type_widen(self)

    /** Follow aliases and dereferences LazyRefs, annotated types and instantiated
     *  TypeVars until type is no longer alias type, annotated type, LazyRef,
     *  or instantiated type variable.
     */
    def dealias(given ctx: Context): Type = internal.Type_dealias(self)

    /** A simplified version of this type which is equivalent wrt =:= to this type.
     *  Reduces typerefs, applied match types, and and or types.
     */
    def simplified(given ctx: Context): Type = internal.Type_simplified(self)

    def classSymbol(given ctx: Context): Option[Symbol] = internal.Type_classSymbol(self)
    def typeSymbol(given ctx: Context): Symbol = internal.Type_typeSymbol(self)
    def termSymbol(given ctx: Context): Symbol = internal.Type_termSymbol(self)
    def isSingleton(given ctx: Context): Boolean = internal.Type_isSingleton(self)
    def memberType(member: Symbol)(given ctx: Context): Type = internal.Type_memberType(self)(member)

    /** Is this type an instance of a non-bottom subclass of the given class `cls`? */
    def derivesFrom(cls: Symbol)(given ctx: Context): Boolean =
      internal.Type_derivesFrom(self)(cls)

    /** Is this type a function type?
     *
     *  @return true if the dealised type of `self` without refinement is `FunctionN[T1, T2, ..., Tn]`
     *
     *  @note The function
     *
     *     - returns true for `given Int => Int` and `erased Int => Int`
     *     - returns false for `List[Int]`, despite that `List[Int] <:< Int => Int`.
     */
    def isFunctionType(given ctx: Context): Boolean = internal.Type_isFunctionType(self)

    /** Is this type an implicit function type?
     *
     *  @see `isFunctionType`
     */
    def isImplicitFunctionType(given ctx: Context): Boolean = internal.Type_isImplicitFunctionType(self)

    /** Is this type an erased function type?
     *
     *  @see `isFunctionType`
     */
    def isErasedFunctionType(given ctx: Context): Boolean = internal.Type_isErasedFunctionType(self)

    /** Is this type a dependent function type?
     *
     *  @see `isFunctionType`
     */
    def isDependentFunctionType(given ctx: Context): Boolean = internal.Type_isDependentFunctionType(self)
  }

  object IsType {
    def unapply(typeOrBounds: TypeOrBounds)(given ctx: Context): Option[Type] =
      internal.matchType(typeOrBounds)
  }

  object Type {
    def apply(clazz: Class[_])(given ctx: Context): Type =
      internal.Type_apply(clazz)
  }

  object IsConstantType {
    /** Matches any ConstantType and returns it */
    def unapply(tpe: TypeOrBounds)(given ctx: Context): Option[ConstantType] =
      internal.matchConstantType(tpe)
  }

  object ConstantType {
    def unapply(typeOrBounds: TypeOrBounds)(given ctx: Context): Option[Constant] =
      internal.matchConstantType(typeOrBounds).map(_.constant)
  }

  object IsTermRef {
    /** Matches any TermRef and returns it */
    def unapply(tpe: TypeOrBounds)(given ctx: Context): Option[TermRef] =
      internal.matchTermRef(tpe)
  }

  object TermRef {
    def apply(qual: TypeOrBounds, name: String)(given ctx: Context): TermRef =
      internal.TermRef_apply(qual, name)
    def unapply(typeOrBounds: TypeOrBounds)(given ctx: Context): Option[(TypeOrBounds /* Type | NoPrefix */, String)] =
      internal.matchTermRef(typeOrBounds).map(x => (x.qualifier, x.name))
  }

  object IsTypeRef {
    /** Matches any TypeRef and returns it */
    def unapply(tpe: TypeOrBounds)(given ctx: Context): Option[TypeRef] =
      internal.matchTypeRef(tpe)
  }

  object TypeRef {
    def unapply(typeOrBounds: TypeOrBounds)(given ctx: Context): Option[(TypeOrBounds /* Type | NoPrefix */, String)] =
      internal.matchTypeRef(typeOrBounds).map(x => (x.qualifier, x.name))
  }

  object IsSuperType {
    /** Matches any SuperType and returns it */
    def unapply(tpe: TypeOrBounds)(given ctx: Context): Option[SuperType] =
      internal.matchSuperType(tpe)
  }

  object SuperType {
    def unapply(typeOrBounds: TypeOrBounds)(given ctx: Context): Option[(Type, Type)] =
      internal.matchSuperType(typeOrBounds).map(x => (x.thistpe, x.supertpe))
  }

  object IsRefinement {
    /** Matches any Refinement and returns it */
    def unapply(tpe: TypeOrBounds)(given ctx: Context): Option[Refinement] =
      internal.matchRefinement(tpe)
  }

  object Refinement {
    def unapply(typeOrBounds: TypeOrBounds)(given ctx: Context): Option[(Type, String, TypeOrBounds /* Type | TypeBounds */)] =
      internal.matchRefinement(typeOrBounds).map(x => (x.parent, x.name, x.info))
  }

  object IsAppliedType {
    /** Matches any AppliedType and returns it */
    def unapply(tpe: TypeOrBounds)(given ctx: Context): Option[AppliedType] =
      internal.matchAppliedType(tpe)
  }

  object AppliedType {
    def apply(tycon: Type, args: List[TypeOrBounds])(given ctx: Context) : AppliedType =
      internal.AppliedType_apply(tycon, args)
    def unapply(typeOrBounds: TypeOrBounds)(given ctx: Context): Option[(Type, List[TypeOrBounds /* Type | TypeBounds */])] =
      internal.matchAppliedType(typeOrBounds).map(x => (x.tycon, x.args))
  }

  object IsAnnotatedType {
    /** Matches any AnnotatedType and returns it */
    def unapply(tpe: TypeOrBounds)(given ctx: Context): Option[AnnotatedType] =
      internal.matchAnnotatedType(tpe)
  }

  object AnnotatedType {
    def unapply(typeOrBounds: TypeOrBounds)(given ctx: Context): Option[(Type, Term)] =
      internal.matchAnnotatedType(typeOrBounds).map(x => (x.underlying, x.annot))
  }

  object IsAndType {
    /** Matches any AndType and returns it */
    def unapply(tpe: TypeOrBounds)(given ctx: Context): Option[AndType] =
      internal.matchAndType(tpe)
  }

  object AndType {
    def unapply(typeOrBounds: TypeOrBounds)(given ctx: Context): Option[(Type, Type)] =
      internal.matchAndType(typeOrBounds).map(x => (x.left, x.right))
  }

  object IsOrType {
    /** Matches any OrType and returns it */
    def unapply(tpe: TypeOrBounds)(given ctx: Context): Option[OrType] =
      internal.matchOrType(tpe)
  }

  object OrType {
    def unapply(typeOrBounds: TypeOrBounds)(given ctx: Context): Option[(Type, Type)] =
      internal.matchOrType(typeOrBounds).map(x => (x.left, x.right))
  }

  object IsMatchType {
    /** Matches any MatchType and returns it */
    def unapply(tpe: TypeOrBounds)(given ctx: Context): Option[MatchType] =
      internal.matchMatchType(tpe)
  }

  object MatchType {
    def unapply(typeOrBounds: TypeOrBounds)(given ctx: Context): Option[(Type, Type, List[Type])] =
      internal.matchMatchType(typeOrBounds).map(x => (x.bound, x.scrutinee, x.cases))
  }

  object IsByNameType {
    /** Matches any ByNameType and returns it */
    def unapply(tpe: TypeOrBounds)(given ctx: Context): Option[ByNameType] =
      internal.matchByNameType(tpe)
  }

  object ByNameType {
    def unapply(typeOrBounds: TypeOrBounds)(given ctx: Context): Option[Type] =
      internal.matchByNameType(typeOrBounds).map(_.underlying)
  }

  object IsParamRef {
    /** Matches any ParamRef and returns it */
    def unapply(tpe: TypeOrBounds)(given ctx: Context): Option[ParamRef] =
      internal.matchParamRef(tpe)
  }

  object ParamRef {
    def unapply(typeOrBounds: TypeOrBounds)(given ctx: Context): Option[(LambdaType[TypeOrBounds], Int)] =
      internal.matchParamRef(typeOrBounds).map(x => (x.binder, x.paramNum))
  }

  object IsThisType {
    /** Matches any ThisType and returns it */
    def unapply(tpe: TypeOrBounds)(given ctx: Context): Option[ThisType] =
      internal.matchThisType(tpe)
  }

  object ThisType {
    def unapply(typeOrBounds: TypeOrBounds)(given ctx: Context): Option[Type] =
      internal.matchThisType(typeOrBounds).map(_.tref)
  }

  object IsRecursiveThis {
    /** Matches any RecursiveThis and returns it */
    def unapply(tpe: TypeOrBounds)(given ctx: Context): Option[RecursiveThis] =
      internal.matchRecursiveThis(tpe)
  }

  object RecursiveThis {
    def unapply(typeOrBounds: TypeOrBounds)(given ctx: Context): Option[RecursiveType] =
      internal.matchRecursiveThis(typeOrBounds).map(_.binder)
  }

  object IsRecursiveType {
    /** Matches any RecursiveType and returns it */
    def unapply(tpe: TypeOrBounds)(given ctx: Context): Option[RecursiveType] =
      internal.matchRecursiveType(tpe)
  }

  object RecursiveType {
    def unapply(typeOrBounds: TypeOrBounds)(given ctx: Context): Option[Type] =
      internal.matchRecursiveType(typeOrBounds).map(_.underlying)
  }

  object IsMethodType {
    /** Matches any MethodType and returns it */
    def unapply(tpe: TypeOrBounds)(given ctx: Context): Option[MethodType] =
      internal.matchMethodType(tpe)
  }

  object MethodType {
    def unapply(typeOrBounds: TypeOrBounds)(given ctx: Context): Option[(List[String], List[Type], Type)] =
      internal.matchMethodType(typeOrBounds).map(x => (x.paramNames, x.paramTypes, x.resType))
  }

  object IsPolyType {
    /** Matches any PolyType and returns it */
    def unapply(tpe: TypeOrBounds)(given ctx: Context): Option[PolyType] =
      internal.matchPolyType(tpe)
  }

  object PolyType {
    def unapply(typeOrBounds: TypeOrBounds)(given ctx: Context): Option[(List[String], List[TypeBounds], Type)] =
      internal.matchPolyType(typeOrBounds).map(x => (x.paramNames, x.paramBounds, x.resType))
  }

  object IsTypeLambda {
    /** Matches any TypeLambda and returns it */
    def unapply(tpe: TypeOrBounds)(given ctx: Context): Option[TypeLambda] =
      internal.matchTypeLambda(tpe)
  }

  object TypeLambda {
    def unapply(typeOrBounds: TypeOrBounds)(given ctx: Context): Option[(List[String], List[TypeBounds], Type)] =
      internal.matchTypeLambda(typeOrBounds).map(x => (x.paramNames, x.paramBounds, x.resType))
  }

  given (self: ConstantType) {
    def constant(given ctx: Context): Constant = internal.ConstantType_constant(self)
  }

  given (self: TermRef) {
    def qualifier(given ctx: Context): TypeOrBounds /* Type | NoPrefix */ = internal.TermRef_qualifier(self)
    def name(given ctx: Context): String = internal.TermRef_name(self)
  }

  given (self: TypeRef) {
    def qualifier(given ctx: Context): TypeOrBounds /* Type | NoPrefix */ = internal.TypeRef_qualifier(self)
    def name(given ctx: Context): String = internal.TypeRef_name(self)
  }

  given (self: SuperType) {
    def thistpe(given ctx: Context): Type = internal.SuperType_thistpe(self)
    def supertpe(given ctx: Context): Type = internal.SuperType_supertpe(self)
  }

  given (self: Refinement) {
    def parent(given ctx: Context): Type = internal.Refinement_parent(self)
    def name(given ctx: Context): String = internal.Refinement_name(self)
    def info(given ctx: Context): TypeOrBounds = internal.Refinement_info(self)
  }

  given (self: AppliedType) {
    def tycon(given ctx: Context): Type = internal.AppliedType_tycon(self)
    def args(given ctx: Context): List[TypeOrBounds /* Type | TypeBounds */] = internal.AppliedType_args(self)
  }

  given (self: AnnotatedType) {
    def underlying(given ctx: Context): Type = internal.AnnotatedType_underlying(self)
    def annot(given ctx: Context): Term = internal.AnnotatedType_annot(self)
  }

  given (self: AndType) {
    def left(given ctx: Context): Type = internal.AndType_left(self)
    def right(given ctx: Context): Type = internal.AndType_right(self)
  }

  given (self: OrType) {
    def left(given ctx: Context): Type = internal.OrType_left(self)
    def right(given ctx: Context): Type = internal.OrType_right(self)
  }

  given (self: MatchType) {
    def bound(given ctx: Context): Type = internal.MatchType_bound(self)
    def scrutinee(given ctx: Context): Type = internal.MatchType_scrutinee(self)
    def cases(given ctx: Context): List[Type] = internal.MatchType_cases(self)
  }

  given (self: ByNameType) {
    def underlying(given ctx: Context): Type = internal.ByNameType_underlying(self)
  }

  given (self: ParamRef) {
    def binder(given ctx: Context): LambdaType[TypeOrBounds] = internal.ParamRef_binder(self)
    def paramNum(given ctx: Context): Int = internal.ParamRef_paramNum(self)
  }

  given (self: ThisType) {
    def tref(given ctx: Context): Type = internal.ThisType_tref(self)
  }

  given (self: RecursiveThis) {
    def binder(given ctx: Context): RecursiveType = internal.RecursiveThis_binder(self)
  }

  given (self: RecursiveType) {
    def underlying(given ctx: Context): Type = internal.RecursiveType_underlying(self)
  }

  given (self: MethodType) {
    def isImplicit: Boolean = internal.MethodType_isImplicit(self)
    def isErased: Boolean = internal.MethodType_isErased(self)
    def paramNames(given ctx: Context): List[String] = internal.MethodType_paramNames(self)
    def paramTypes(given ctx: Context): List[Type] = internal.MethodType_paramTypes(self)
    def resType(given ctx: Context): Type = internal.MethodType_resType(self)
  }

  given (self: PolyType) {
    def paramNames(given ctx: Context): List[String] = internal.PolyType_paramNames(self)
    def paramBounds(given ctx: Context): List[TypeBounds] = internal.PolyType_paramBounds(self)
    def resType(given ctx: Context): Type = internal.PolyType_resType(self)
  }

  given (self: TypeLambda) {
    def paramNames(given ctx: Context): List[String] = internal.TypeLambda_paramNames(self)
    def paramBounds(given ctx: Context): List[TypeBounds] = internal.TypeLambda_paramBounds(self)
    def resType(given ctx: Context): Type = internal.TypeLambda_resType(self)
  }

  // ----- TypeBounds -----------------------------------------------

  object IsTypeBounds {
    def unapply(typeOrBounds: TypeOrBounds)(given ctx: Context): Option[TypeBounds] =
      internal.matchTypeBounds(typeOrBounds)
  }

  object TypeBounds {
    def unapply(typeOrBounds: TypeOrBounds)(given ctx: Context): Option[(Type, Type)] =
      internal.matchTypeBounds(typeOrBounds).map(x => (x.low, x.hi))
  }

  given (self: TypeBounds) {
    def low(given ctx: Context): Type = internal.TypeBounds_low(self)
    def hi(given ctx: Context): Type = internal.TypeBounds_hi(self)
  }

  // ----- NoPrefix -------------------------------------------------

  object NoPrefix {
    def unapply(typeOrBounds: TypeOrBounds)(given ctx: Context): Boolean =
      internal.matchNoPrefix(typeOrBounds).isDefined
  }

}
