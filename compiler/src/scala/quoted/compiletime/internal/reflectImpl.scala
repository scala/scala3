package scala.quoted.compiletime
package internal

import dotty.tools.dotc.core.Contexts.*

object reflectImpl {

  final class Module(using val ctx: Context) extends reflect.Module {

    override lazy val Symbol: SymbolImpl.Module = new SymbolImpl.Module
    override lazy val Position: PositionImpl.Module = new PositionImpl.Module
    override lazy val SourceFile: SourceFileImpl.Module = new SourceFileImpl.Module

    override lazy val Constant: ConstantImpl.Module.type = ConstantImpl.Module
    override lazy val BooleanConstant: BooleanConstantImpl.Module.type = BooleanConstantImpl.Module
    override lazy val ByteConstant: ByteConstantImpl.Module.type = ByteConstantImpl.Module
    override lazy val ShortConstant: ShortConstantImpl.Module.type = ShortConstantImpl.Module
    override lazy val IntConstant: IntConstantImpl.Module.type = IntConstantImpl.Module
    override lazy val LongConstant: LongConstantImpl.Module.type = LongConstantImpl.Module
    override lazy val FloatConstant: FloatConstantImpl.Module.type = FloatConstantImpl.Module
    override lazy val DoubleConstant: DoubleConstantImpl.Module.type = DoubleConstantImpl.Module
    override lazy val CharConstant: CharConstantImpl.Module.type = CharConstantImpl.Module
    override lazy val StringConstant: StringConstantImpl.Module.type = StringConstantImpl.Module
    override lazy val UnitConstant: UnitConstantImpl.Module.type = UnitConstantImpl.Module
    override lazy val NullConstant: NullConstantImpl.Module.type = NullConstantImpl.Module
    override lazy val ClassOfConstant: ClassOfConstantImpl.Module.type = ClassOfConstantImpl.Module

    override lazy val TypeRepr: TypeReprImpl.Module.type = TypeReprImpl.Module
    override lazy val NamedType: NamedTypeImpl.Module.type = NamedTypeImpl.Module
    override lazy val TermRef: TermRefImpl.Module.type = TermRefImpl.Module
    override lazy val TypeRef: TypeRefImpl.Module.type = TypeRefImpl.Module
    override lazy val ConstantType: ConstantTypeImpl.Module.type = ConstantTypeImpl.Module
    override lazy val SuperType: SuperTypeImpl.Module.type = SuperTypeImpl.Module
    override lazy val Refinement: RefinementImpl.Module.type = RefinementImpl.Module
    override lazy val AppliedType: AppliedTypeImpl.Module.type = AppliedTypeImpl.Module
    override lazy val AnnotatedType: AnnotatedTypeImpl.Module.type = AnnotatedTypeImpl.Module
    override lazy val AndOrType: AndOrTypeImpl.Module.type = AndOrTypeImpl.Module
    override lazy val AndType: AndTypeImpl.Module.type = AndTypeImpl.Module
    override lazy val OrType: OrTypeImpl.Module.type = OrTypeImpl.Module
    override lazy val MatchType: MatchTypeImpl.Module.type = MatchTypeImpl.Module
    override lazy val ByNameType: ByNameTypeImpl.Module.type = ByNameTypeImpl.Module
    override lazy val ParamRef: ParamRefImpl.Module.type = ParamRefImpl.Module
    override lazy val ThisType: ThisTypeImpl.Module.type = ThisTypeImpl.Module
    override lazy val RecursiveThis: RecursiveThisImpl.Module.type = RecursiveThisImpl.Module
    override lazy val RecursiveType: RecursiveTypeImpl.Module.type = RecursiveTypeImpl.Module
    override lazy val LambdaType: LambdaTypeImpl.Module.type = LambdaTypeImpl.Module
    override lazy val MethodOrPoly: MethodOrPolyImpl.Module.type = MethodOrPolyImpl.Module
    override lazy val MethodType: MethodTypeImpl.Module.type = MethodTypeImpl.Module
    override lazy val PolyType: PolyTypeImpl.Module.type = PolyTypeImpl.Module
    override lazy val TypeLambda: TypeLambdaImpl.Module.type = TypeLambdaImpl.Module
    override lazy val MatchCase: MatchCaseImpl.Module.type = MatchCaseImpl.Module
    override lazy val TypeBounds: TypeBoundsImpl.Module.type = TypeBoundsImpl.Module
    override lazy val NoPrefix: NoPrefixImpl.Module.type = NoPrefixImpl.Module
    override lazy val FlexibleType: FlexibleTypeImpl.Module.type = FlexibleTypeImpl.Module

    override lazy val Signature: SignatureImpl.Module.type = SignatureImpl.Module
    override lazy val Flags: FlagsImpl.Module.type = FlagsImpl.Module

    override lazy val Selector: SelectorImpl.Module.type = SelectorImpl.Module
    override lazy val SimpleSelector: SimpleSelectorImpl.Module.type = SimpleSelectorImpl.Module
    override lazy val RenameSelector: RenameSelectorImpl.Module.type = RenameSelectorImpl.Module
    override lazy val OmitSelector: OmitSelectorImpl.Module.type = OmitSelectorImpl.Module
    override lazy val GivenSelector: GivenSelectorImpl.Module.type = GivenSelectorImpl.Module

    override lazy val ParamClause: ParamClauseImpl.Module.type = ParamClauseImpl.Module
    override lazy val TermParamClause: TermParamClauseImpl.Module.type = TermParamClauseImpl.Module
    override lazy val TypeParamClause: TypeParamClauseImpl.Module.type = TypeParamClauseImpl.Module

    override lazy val Tree: TreeImpl.Module.type = TreeImpl.Module
    override lazy val PackageClause: PackageClauseImpl.Module.type = PackageClauseImpl.Module
    override lazy val Statement: StatementImpl.Module.type = StatementImpl.Module
    override lazy val Import: ImportImpl.Module.type = ImportImpl.Module
    override lazy val Export: ExportImpl.Module.type = ExportImpl.Module
    override lazy val Definition: DefinitionImpl.Module.type = DefinitionImpl.Module
    override lazy val ClassDef: ClassDefImpl.Module.type = ClassDefImpl.Module
    override lazy val ValOrDefDef: ValOrDefDefImpl.Module.type = ValOrDefDefImpl.Module
    override lazy val DefDef: DefDefImpl.Module.type = DefDefImpl.Module
    override lazy val ValDef: ValDefImpl.Module.type = ValDefImpl.Module
    override lazy val TypeDef: TypeDefImpl.Module.type = TypeDefImpl.Module
    override lazy val Term: TermImpl.Module.type = TermImpl.Module
    override lazy val Ref: RefImpl.Module.type = RefImpl.Module
    override lazy val Ident: IdentImpl.Module.type = IdentImpl.Module
    override lazy val Wildcard: WildcardImpl.Module.type = WildcardImpl.Module
    override lazy val Select: SelectImpl.Module.type = SelectImpl.Module
    override lazy val Literal: LiteralImpl.Module.type = LiteralImpl.Module
    override lazy val This: ThisImpl.Module.type = ThisImpl.Module
    override lazy val New: NewImpl.Module.type = NewImpl.Module
    override lazy val NamedArg: NamedArgImpl.Module.type = NamedArgImpl.Module
    override lazy val Apply: ApplyImpl.Module.type = ApplyImpl.Module
    override lazy val TypeApply: TypeApplyImpl.Module.type = TypeApplyImpl.Module
    override lazy val Super: SuperImpl.Module.type = SuperImpl.Module
    override lazy val Typed: TypedImpl.Module.type = TypedImpl.Module
    override lazy val Assign: AssignImpl.Module.type = AssignImpl.Module
    override lazy val Block: BlockImpl.Module.type = BlockImpl.Module
    override lazy val Closure: ClosureImpl.Module.type = ClosureImpl.Module
    override lazy val Lambda: LambdaImpl.Module.type = LambdaImpl.Module
    override lazy val If: IfImpl.Module.type = IfImpl.Module
    override lazy val Match: MatchImpl.Module.type = MatchImpl.Module
    override lazy val SummonFrom: SummonFromImpl.Module.type = SummonFromImpl.Module
    override lazy val Try: TryImpl.Module.type = TryImpl.Module
    override lazy val Return: ReturnImpl.Module.type = ReturnImpl.Module
    override lazy val Repeated: RepeatedImpl.Module.type = RepeatedImpl.Module
    override lazy val Inlined: InlinedImpl.Module.type = InlinedImpl.Module
    override lazy val SelectOuter: SelectOuterImpl.Module.type = SelectOuterImpl.Module
    override lazy val While: WhileImpl.Module.type = WhileImpl.Module
    override lazy val TypedOrTest: TypedOrTestImpl.Module.type = TypedOrTestImpl.Module
    override lazy val TypeTree: TypeTreeImpl.Module.type = TypeTreeImpl.Module
    override lazy val Inferred: InferredImpl.Module.type = InferredImpl.Module
    override lazy val TypeIdent: TypeIdentImpl.Module.type = TypeIdentImpl.Module
    override lazy val TypeSelect: TypeSelectImpl.Module.type = TypeSelectImpl.Module
    override lazy val TypeProjection: TypeProjectionImpl.Module.type = TypeProjectionImpl.Module
    override lazy val Singleton: SingletonImpl.Module.type = SingletonImpl.Module
    override lazy val Refined: RefinedImpl.Module.type = RefinedImpl.Module
    override lazy val Applied: AppliedImpl.Module.type = AppliedImpl.Module
    override lazy val Annotated: AnnotatedImpl.Module.type = AnnotatedImpl.Module
    override lazy val MatchTypeTree: MatchTypeTreeImpl.Module.type = MatchTypeTreeImpl.Module
    override lazy val ByName: ByNameImpl.Module.type = ByNameImpl.Module
    override lazy val LambdaTypeTree: LambdaTypeTreeImpl.Module.type = LambdaTypeTreeImpl.Module
    override lazy val TypeBind: TypeBindImpl.Module.type = TypeBindImpl.Module
    override lazy val TypeBlock: TypeBlockImpl.Module.type = TypeBlockImpl.Module
    override lazy val TypeBoundsTree: TypeBoundsTreeImpl.Module.type = TypeBoundsTreeImpl.Module
    override lazy val WildcardTypeTree: WildcardTypeTreeImpl.Module.type = WildcardTypeTreeImpl.Module
    override lazy val CaseDef: CaseDefImpl.Module.type = CaseDefImpl.Module
    override lazy val TypeCaseDef: TypeCaseDefImpl.Module.type = TypeCaseDefImpl.Module
    override lazy val Bind: BindImpl.Module.type = BindImpl.Module
    override lazy val Unapply: UnapplyImpl.Module.type = UnapplyImpl.Module
    override lazy val Alternatives: AlternativesImpl.Module.type = AlternativesImpl.Module

  }

}