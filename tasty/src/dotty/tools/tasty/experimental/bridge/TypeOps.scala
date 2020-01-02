package dotty.tools.tasty.experimental.bridge

import reflect.ClassTag

trait TypeOps extends Core with

  given ClassTag[Type] = internal.Type_CT
  given ClassTag[AppliedType] = internal.AppliedType_CT
  given ClassTag[ConstantType] = internal.ConstantType_CT
  given ClassTag[NamedType] = internal.NamedType_CT
  given ClassTag[ThisType] = internal.ThisType_CT
  given ClassTag[SuperType] = internal.SuperType_CT
  given ClassTag[RecThis] = internal.RecThis_CT
  given ClassTag[RecType] = internal.RecType_CT
  given ClassTag[ParamRef] = internal.ParamRef_CT
  given ClassTag[SkolemType] = internal.SkolemType_CT
  given ClassTag[RefinedType] = internal.RefinedType_CT
  given ClassTag[TypeAlias] = internal.TypeAlias_CT
  given ClassTag[TypeBounds] = internal.TypeBounds_CT
  given ClassTag[AnnotatedType] = internal.AnnotatedType_CT
  given ClassTag[AndType] = internal.AndType_CT
  given ClassTag[OrType] = internal.OrType_CT
  given ClassTag[MatchType] = internal.MatchType_CT
  given ClassTag[ExprType] = internal.ExprType_CT
  given ClassTag[HKTypeLambda] = internal.HKTypeLambda_CT
  given ClassTag[PolyType] = internal.PolyType_CT
  given ClassTag[MethodType] = internal.MethodType_CT
  given ClassTag[TermRef] = internal.TermRef_CT
  given ClassTag[TypeRef] = internal.TypeRef_CT
  given ClassTag[LazyRef] = internal.LazyRef_CT
  given ClassTag[ClassInfo] = internal.ClassInfo_CT

  object AppliedType with
    def unapply(tpe: AppliedType): (Type, List[Type]) = internal.AppliedType_unapply(tpe)

  given TypeOps: (tpe: Type) extended with
    def stripTypeVar(given Context): Type = internal.Type_stripTypeVar(tpe)
    def signature(given Context): Signature = internal.Type_signature(tpe)
    def member(name: Name)(given Context): Symbol = internal.Type_member(tpe, name)
    def isContextualMethod: Boolean = internal.Type_isContextualMethod(tpe)
    def isImplicitMethod: Boolean = internal.Type_isImplicitMethod(tpe)
    def isErasedMethod: Boolean = internal.Type_isErasedMethod(tpe)
    def exists: Boolean = internal.Type_exists(tpe)

  given ConstantTypeOps: (tpe: ConstantType) extended with
    def value: Constant = internal.ConstantType_value(tpe)

  given SuperTypeOps: (tpe: SuperType) extended with
    def thistpe: Type = internal.SuperType_thistpe(tpe)
    def supertpe: Type = internal.SuperType_supertpe(tpe)

  given ThisTypeOps: (tpe: ThisType) extended with
    def cls(given Context): ClassSymbol = internal.ThisType_cls(tpe)
    def tref: TypeRef = internal.ThisType_tref(tpe)

  given SkolemTypeOps: (tpe: SkolemType) extended with
    def info: Type = internal.SkolemType_info(tpe)

  given BoundTypeOps: (tpe: BoundType) extended with
    def binder: tpe.BT = internal.BoundType_binder(tpe)

  given ParamRefOps: (tpe: ParamRef) extended with
    def paramNum: Int = internal.ParamRef_paramNum(tpe)

  given RecTypeOps: (tpe: RecType) extended with
    def parent: Type = internal.RecType_parent(tpe)

  given RefinedTypeOps: (tpe: RefinedType) extended with
    def parent: Type = internal.RefinedType_parent(tpe)
    def refinedInfo: Type = internal.RefinedType_refinedInfo(tpe)
    def refinedName: Name = internal.RefinedType_refinedName(tpe)

  given TypeBoundsOps: (tpe: TypeBounds) extended with
    def hi: Type = internal.TypeBounds_hi(tpe)
    def lo: Type = internal.TypeBounds_lo(tpe)

  given TypeAliasOps: (tpe: TypeAlias) extended with
    def alias: Type = internal.TypeAlias_alias(tpe)

  given NamedTypeOps: (tpe: NamedType) extended with
    def symbol(given Context): Symbol = internal.NamedType_symbol(tpe)
    def prefix: Type = internal.NamedType_prefix(tpe)
    def designator: Designator = internal.NamedType_designator(tpe)
    def hasNoPrefix: Boolean = internal.NamedType_hasNoPrefix(tpe)
    def isType: Boolean = internal.NamedType_isType(tpe)

  given AnnotatedTypeOps: (tpe: AnnotatedType) extended with
    def parent: Type = internal.AnnotatedType_parent(tpe)
    def annot: Annotation = internal.AnnotatedType_annot(tpe)

  given AndOrTypeOps: (tpe: AndOrType) extended with
    def tp1: Type = internal.AndOrType_tp1(tpe)
    def tp2: Type = internal.AndOrType_tp2(tpe)

  given TypeProxyOps: (tpe: TypeProxy) extended with
    def underlying(given Context): Type = internal.TypeProxy_underlying(tpe)

  given MatchTypeOps: (tpe: MatchType) extended with
    def bound: Type = internal.MatchType_bound(tpe)
    def scrutinee: Type = internal.MatchType_scrutinee(tpe)
    def cases: List[Type] = internal.MatchType_cases(tpe)

  given LambdaTypeOps: (tpe: LambdaType) extended with
    def resultType(given Context): Type = internal.LambdaType_resultType(tpe)
    def paramNames: List[tpe.ThisName] = internal.LambdaType_paramNames(tpe)
    def paramInfos: List[tpe.PInfo] = internal.LambdaType_paramInfos(tpe)

  given LazyRefOps: (tpe: LazyRef) extended with
    def ref(given Context): Type = internal.LazyRef_ref(tpe)

  given ClassInfoOps: (tpe: ClassInfo) extended with
    def selfInfo: Either[Type, Symbol] = internal.ClassInfo_selfInfo(tpe)
