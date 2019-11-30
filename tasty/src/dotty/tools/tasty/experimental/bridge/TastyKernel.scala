package dotty.tools.tasty.experimental.bridge

import dotty.tools.tasty.experimental.function._

import reflect.ClassTag

import java.nio.file.Path

trait TastyKernel with

  type Context <: AnyRef

  type SourceFile <: AnyRef

  type Designator <: AnyRef

  type Annotation <: AnyRef
  val Annotation_CT: ClassTag[Annotation]

  type Name <: Designator
  type SimpleName <: TermName
  type DerivedName <: TermName
  type TypeName <: Name
  type TermName <: Name

  val Name_CT: ClassTag[Name]
  val SimpleName_CT: ClassTag[SimpleName]
  val DerivedName_CT: ClassTag[DerivedName]
  val TypeName_CT: ClassTag[TypeName]
  val TermName_CT: ClassTag[TermName]

  type Signature <: AnyRef

  val Signature_CT: ClassTag[Signature]

  type Signature_ParamSig = Int | TypeName

  type Positioned <: AnyRef

  type untpd_Tree <: Product with Positioned
  type untpd_ImportSelector <: untpd_Tree
  type untpd_TypedSplice <: untpd_Tree
  type untpd_MemberDef <: untpd_Tree

  type Tree <: untpd_Tree { type ThisTree <: Tree }
  type MemberDef <: Tree
  type Hole <: Tree
  type Template <: Tree // DefTree
  type ValOrDefDef <: Tree // DefTree
  type TypeDef <: MemberDef
  type ValDef <: ValOrDefDef
  type DefDef <: ValOrDefDef
  type RefTree <: Tree
  type Ident <: RefTree
  type This <: Tree // DenotingTree[T] with TermTree[T]
  type Select <: RefTree
  type Apply <: Tree // GenericApply -> TermTree
  type TypeApply <: Tree // GenericApply -> TermTree
  type Literal <: Tree // TermTree
  type Super <: Tree // TermTree
  type New <: Tree // TermTree
  type Typed <: Tree // TermTree
  type NamedArg <: Tree
  type Assign <: Tree // TermTree
  type Block <: Tree
  type If <: Tree // TermTree
  type Closure <: Tree // TermTree
  type Match <: Tree // TermTree
  type CaseDef <: Tree
  type Labeled <: Tree // NameTree
  type Return <: Tree // TermTree
  type WhileDo <: Tree // TermTree
  type Try <: Tree // TermTree
  type SeqLiteral <: Tree // TermTree
  type Inlined <: Tree
  type Bind <: Tree // NameTree with DefTree with PatternTree
  type Alternative <: Tree // PatternTree
  type UnApply <: Tree // PatternTree
  type Import <: Tree // DenotingTree
  type PackageDef <: Tree
  type TypeTree <: Tree // DenotingTree[T] with TypTree[T]
  type SingletonTypeTree <: Tree // DenotingTree[T] with TypTree[T]
  type RefinedTypeTree <: Tree // TypTree[T]
  type AppliedTypeTree <: Tree // TypTree[T]
  type MatchTypeTree <: Tree // TypTree[T]
  type ByNameTypeTree <: Tree // TypTree[T]
  type Annotated <: Tree
  type LambdaTypeTree <: Tree // TypTree[T]
  type TypeBoundsTree <: Tree // TypTree[T]
  type Thicket <: Tree

  val untpd_Tree_CT: ClassTag[untpd_Tree]
  val untpd_TypedSplice_CT: ClassTag[untpd_TypedSplice]
  val untpd_MemberDef_CT: ClassTag[untpd_MemberDef]

  val Tree_CT: ClassTag[Tree]
  val MemberDef_CT: ClassTag[MemberDef]
  val Hole_CT: ClassTag[Hole]
  val Template_CT: ClassTag[Template]
  val ValOrDefDef_CT: ClassTag[ValOrDefDef]
  val TypeDef_CT: ClassTag[TypeDef]
  val ValDef_CT: ClassTag[ValDef]
  val DefDef_CT: ClassTag[DefDef]
  val Ident_CT: ClassTag[Ident]
  val This_CT: ClassTag[This]
  val Select_CT: ClassTag[Select]
  val Apply_CT: ClassTag[Apply]
  val TypeApply_CT: ClassTag[TypeApply]
  val Literal_CT: ClassTag[Literal]
  val Super_CT: ClassTag[Super]
  val New_CT: ClassTag[New]
  val Typed_CT: ClassTag[Typed]
  val NamedArg_CT: ClassTag[NamedArg]
  val Assign_CT: ClassTag[Assign]
  val Block_CT: ClassTag[Block]
  val If_CT: ClassTag[If]
  val Closure_CT: ClassTag[Closure]
  val Match_CT: ClassTag[Match]
  val CaseDef_CT: ClassTag[CaseDef]
  val Labeled_CT: ClassTag[Labeled]
  val Return_CT: ClassTag[Return]
  val WhileDo_CT: ClassTag[WhileDo]
  val Try_CT: ClassTag[Try]
  val SeqLiteral_CT: ClassTag[SeqLiteral]
  val Inlined_CT: ClassTag[Inlined]
  val Bind_CT: ClassTag[Bind]
  val Alternative_CT: ClassTag[Alternative]
  val UnApply_CT: ClassTag[UnApply]
  val Import_CT: ClassTag[Import]
  val PackageDef_CT: ClassTag[PackageDef]
  val TypeTree_CT: ClassTag[TypeTree]
  val SingletonTypeTree_CT: ClassTag[SingletonTypeTree]
  val RefinedTypeTree_CT: ClassTag[RefinedTypeTree]
  val AppliedTypeTree_CT: ClassTag[AppliedTypeTree]
  val MatchTypeTree_CT: ClassTag[MatchTypeTree]
  val ByNameTypeTree_CT: ClassTag[ByNameTypeTree]
  val Annotated_CT: ClassTag[Annotated]
  val LambdaTypeTree_CT: ClassTag[LambdaTypeTree]
  val TypeBoundsTree_CT: ClassTag[TypeBoundsTree]
  val Thicket_CT: ClassTag[Thicket]

  type Type <: AnyRef
  type AppliedType <: Type // <: CachedProxyType with ValueType
  type ConstantType <: Type  // <: CachedProxyType with SingletonType
  type ClassInfo <: Type // TypeType
  type NamedType <: Type  // <: CachedProxyType with SingletonType
  type ThisType <: Type // SingletonType
  type SuperType <: Type // SingletonType
  type BoundType <: Type { type BT <: Type }
  type RecThis <: BoundType { type BT = RecType } // ValueType
  type ParamRef <: BoundType { type BT <: LambdaType }
  type RecType <: Type // RefinedOrRecType with BindingType
  type RefinedType <: Type
  type SkolemType <: Type // SingletonType
  type TypeBounds <: Type
  type TypeAlias <: TypeBounds // AliasingBounds
  type TermRef <: NamedType // SingletonType
  type TypeRef <: NamedType // SingletonType
  type AnnotatedType <: Type
  type AndOrType <: Type
  type AndType <: AndOrType
  type OrType <: AndOrType
  type TypeProxy <: Type
  type ExprType <: TypeProxy // Methodic
  type MatchType <: Type
  type LambdaType <: Type { type ThisName <: Name; type PInfo <: Type }
  type HKTypeLambda <: LambdaType // HKLambda with TypeLambda
  type PolyType <: LambdaType // MethodOrPoly with TypeLambda
  type MethodType <: LambdaType // MethodOrPoly with TermLambda
  type LazyRef <: Type

  val Type_CT: ClassTag[Type]
  val AppliedType_CT: ClassTag[AppliedType]
  val ConstantType_CT: ClassTag[ConstantType]
  val NamedType_CT: ClassTag[NamedType]
  val ThisType_CT: ClassTag[ThisType]
  val SuperType_CT: ClassTag[SuperType]
  val RecThis_CT: ClassTag[RecThis]
  val RecType_CT: ClassTag[RecType]
  val TermRef_CT: ClassTag[TermRef]
  val TypeRef_CT: ClassTag[TypeRef]
  val ParamRef_CT: ClassTag[ParamRef]
  val SkolemType_CT: ClassTag[SkolemType]
  val RefinedType_CT: ClassTag[RefinedType]
  val TypeAlias_CT: ClassTag[TypeAlias]
  val TypeBounds_CT: ClassTag[TypeBounds]
  val AnnotatedType_CT: ClassTag[AnnotatedType]
  val AndType_CT: ClassTag[AndType]
  val OrType_CT: ClassTag[OrType]
  val MatchType_CT: ClassTag[MatchType]
  val ExprType_CT: ClassTag[ExprType]
  val HKTypeLambda_CT: ClassTag[HKTypeLambda]
  val PolyType_CT: ClassTag[PolyType]
  val MethodType_CT: ClassTag[MethodType]
  val LazyRef_CT: ClassTag[LazyRef]
  val ClassInfo_CT: ClassTag[ClassInfo]

  type Symbol <: Designator { type ThisName <: Name }
  type TermSymbol <: Symbol { type ThisName = TermName }
  type TypeSymbol <: Symbol { type ThisName = TypeName }
  type ClassSymbol <: TypeSymbol

  type FlagSet <: AnyVal
  type Flag <: FlagSet

  val Symbol_CT: ClassTag[Symbol]
  val ClassSymbol_CT: ClassTag[ClassSymbol]

  type Symbols_MutableSymbolMap[T] <: AnyRef

  type SourcePosition <: AnyRef
  type Span <: AnyVal

  type ContextDocstrings <: AnyRef

  type Comment <: AnyRef

  type Constant <: AnyVal

  val Flags_Protected: Flag
  val Flags_ParamAccessor: Flag
  val Flags_Private: Flag
  val Flags_Final: Flag
  val Flags_Case: Flag
  val Flags_Override: Flag
  val Flags_Inline: Flag
  val Flags_InlineProxy: Flag
  val Flags_Macro: Flag
  val Flags_JavaStatic: Flag
  val Flags_Module: Flag
  val Flags_Enum: Flag
  val Flags_Local: Flag
  val Flags_Synthetic: Flag
  val Flags_Artifact: Flag
  val Flags_Scala2x: Flag
  val Flags_Implicit: Flag
  val Flags_Given: Flag
  val Flags_Erased: Flag
  val Flags_Lazy: Flag
  val Flags_AbsOverride: Flag
  val Flags_Mutable: Flag
  val Flags_Accessor: Flag
  val Flags_CaseAccessor: Flag
  val Flags_DefaultParameterized: Flag
  val Flags_StableRealizable: Flag
  val Flags_Extension: Flag
  val Flags_Exported: Flag
  val Flags_Label: Flag
  val Flags_Sealed: Flag
  val Flags_Abstract: Flag
  val Flags_Trait: Flag
  val Flags_Covariant: Flag
  val Flags_Contravariant: Flag
  val Flags_Opaque: Flag
  val Flags_Open: Flag

  def FlagSet_is(flags: FlagSet, flag: Flag): Boolean
  def FlagSet_is(flags: FlagSet, flag: Flag, butNot: FlagSet): Boolean
  def FlagSet_&~(flags: FlagSet, flag: Flag): FlagSet

  final val Constants_NoTag      = 0
  final val Constants_UnitTag    = 1
  final val Constants_BooleanTag = 2
  final val Constants_ByteTag    = 3
  final val Constants_ShortTag   = 4
  final val Constants_CharTag    = 5
  final val Constants_IntTag     = 6
  final val Constants_LongTag    = 7
  final val Constants_FloatTag   = 8
  final val Constants_DoubleTag  = 9
  final val Constants_StringTag  = 10
  final val Constants_NullTag    = 11
  final val Constants_ClazzTag   = 12
  final val Constants_EnumTag    = 13

  def Context_log(ctx: Context, msg: => String, sourcePos: SourcePosition): Unit
  def Context_source(ctx: Context): SourceFile
  def Context_docCtx(ctx: Context): Option[ContextDocstrings]
  def Context_withOwner(ctx: Context, owner: Symbol): Context
  def Context_withSource(ctx: Context, source: SourceFile): Context

  def ContextDocstrings_docString(ctx: ContextDocstrings, sym: Symbol): Option[Comment]

  def Constant_tag(c: Constant): Int
  def Constant_intValue(c: Constant): Int
  def Constant_booleanValue(c: Constant): Boolean
  def Constant_byteValue(c: Constant): Byte
  def Constant_charValue(c: Constant): Char
  def Constant_shortValue(c: Constant): Short
  def Constant_longValue(c: Constant): Long
  def Constant_doubleValue(c: Constant): Double
  def Constant_floatValue(c: Constant): Float
  def Constant_stringValue(c: Constant): String
  def Constant_typeValue(c: Constant): Type
  def Constant_symbolValue(c: Constant): Symbol

  def Symbols_MutableSymbolMap_get[T](map: Symbols_MutableSymbolMap[T], sym: Symbol): Option[T]
  def Symbols_MutableSymbolMap_getOrElse[U >: T, T](map: Symbols_MutableSymbolMap[T], sym: Symbol, default: => U): U
  def Symbols_MutableSymbolMap_contains[T](map: Symbols_MutableSymbolMap[T], sym: Symbol): Boolean
  def Symbols_MutableSymbolMap_update[T](map: Symbols_MutableSymbolMap[T], sym: Symbol, value: T): Unit
  def Symbols_MutableSymbolMap_-=[T](map: Symbols_MutableSymbolMap[T], sym: Symbol): Unit
  def Symbols_MutableSymbolMap_apply[T](map: Symbols_MutableSymbolMap[T], sym: Symbol): T
  def Symbols_MutableSymbolMap_keysIterator[T](map: Symbols_MutableSymbolMap[T]): Iterator[Symbol]
  def Symbols_MutableSymbolMap_isEmpty[T](map: Symbols_MutableSymbolMap[T]): Boolean
  def Symbols_newMutableSymbolMap[A]: Symbols_MutableSymbolMap[A]

  def Symbol_isPackage(sym: Symbol)(given Context): Boolean
  def Symbol_isPrivate(sym: Symbol)(given Context): Boolean
  def Symbol_sourcePos(sym: Symbol)(given Context): SourcePosition
  def Symbol_owner(sym: Symbol): Symbol
  def Symbol_isDefinedWithin(sym: Symbol, outer: Symbol)(given Context): Boolean
  def Symbol_termRef(sym: Symbol)(given Context): TermRef
  def Symbol_typeRef(sym: Symbol)(given Context): TypeRef
  def Symbol_name(sym: Symbol)(given Context): sym.ThisName
  def Symbol_fullName(sym: Symbol)(given Context): sym.ThisName
  def Symbol_isClass(sym: Symbol): Boolean
  def Symbol_exists(sym: Symbol): Boolean
  def Symbol_isEffectiveRoot(sym: Symbol)(given Context): Boolean
  def Symbol_flags(sym: Symbol)(given Context): FlagSet
  def Symbol_privateWithin(sym: Symbol)(given Context): Symbol
  def Symbol_isTerm(sym: Symbol)(given Context): Boolean
  def Symbol_isSetter(sym: Symbol)(given Context): Boolean
  def Symbol_info(sym: Symbol)(given Context): Type
  def Symbol_showLocated(sym: Symbol)(given Context): String
  def Symbol_annotations(sym: Symbol)(given Context): List[Annotation]
  def Symbol_isInaccessibleChildOf(sym: Symbol, cls: Symbol)(given Context): Boolean

  def SourceFile_path(source: SourceFile): String
  def SourceFile_exists(source: SourceFile): Boolean
  val SourceFile_noSource: SourceFile

  def SourcePosition_line(pos: SourcePosition): Int

  val Span_empty: Span
  val Span_noSpan: Span
  def Span_start(span: Span): Int
  def Span_end(span: Span): Int
  def Span_isSynthetic(span: Span): Boolean
  def Span_toSynthetic(span: Span): Span
  def Span_pointDelta(span: Span): Int
  def Span_coords(span: Span): Long
  def Span_exists(span: Span): Boolean

  def defn_throwMethod(given Context): TermSymbol
  def defn_BodyAnnot(given Context): ClassSymbol

  def Name_toTermName(name: Name): TermName
  def Name_isEmpty(name: Name): Boolean
  def Name_isTypeName(name: Name): Boolean
  def Name_length(name: Name): Int

  def Positioned_alwaysNeedsPos(positioned: Positioned): Boolean

  def untpd_Tree_span(tree: untpd_Tree): Span
  def untpd_Tree_source(tree: untpd_Tree): SourceFile
  def untpd_Tree_envelope(tree: untpd_Tree, src: SourceFile, startSpan: Span): Span
  def untpd_Tree_symbol(tree: untpd_Tree)(given Context): Symbol

  def Tree_withType(tree: Tree, tpe: Type)(given Context): tree.ThisTree
  def Tree_isEmpty(tree: Tree): Boolean
  def Tree_isType(tree: Tree): Boolean
  def Tree_isInline(tree: Tree): Boolean
  def Tree_tpe(tree: Tree): Type
  val EmptyTree: Tree

  def inlineContext(tree: Tree)(implicit ctx: Context): Context

  def TypedSplice_unapply(tree: untpd_TypedSplice): Some[Tree]
  def Ident_unapply(tree: Ident): Some[Name]
  def This_unapply(tree: This): Some[Ident]
  def Select_unapply(tree: Select): (Tree, Name)
  def Apply_unapply(tree: Apply): (Tree, List[Tree])
  def TypeApply_unapply(tree: TypeApply): (Tree, List[Tree])
  def Literal_unapply(tree: Literal): Some[Constant]
  def Super_unapply(tree: Super): (Tree, Ident)
  def New_unapply(tree: New): Some[Tree]
  def Typed_unapply(tree: Typed): (Tree, Tree)
  def NamedArg_unapply(tree: NamedArg): (Name, Tree)
  def Assign_unapply(tree: Assign): (Tree, Tree)
  def Block_unapply(tree: Block): (List[Tree], Tree)
  def If_unapply(tree: If): (Tree, Tree, Tree)
  def Closure_unapply(tree: Closure): (List[Tree], Tree, Tree)
  def Match_unapply(tree: Match): (Tree, List[CaseDef])
  def CaseDef_unapply(tree: CaseDef): (Tree, Tree, Tree)
  def Labeled_unapply(tree: Labeled): (Bind, Tree)
  def Return_unapply(tree: Return): (Tree, Tree)
  def WhileDo_unapply(tree: WhileDo): (Tree, Tree)
  def Try_unapply(tree: Try): (Tree, List[CaseDef], Tree)
  def SeqLiteral_unapply(tree: SeqLiteral): (List[Tree], Tree)
  def Inlined_unapply(tree: Inlined): (Tree, List[MemberDef], Tree)
  def Bind_unapply(tree: Bind): (Name, Tree)
  def Alternative_unapply(tree: Alternative): Some[List[Tree]]
  def UnApply_unapply(tree: UnApply): (Tree, List[Tree], List[Tree])
  def Import_unapply(tree: Import): (Tree, List[untpd_ImportSelector])
  def PackageDef_unapply(tree: PackageDef): (RefTree, List[Tree])
  def SingletonTypeTree_unapply(tree: SingletonTypeTree): Some[Tree]
  def RefinedTypeTree_unapply(tree: RefinedTypeTree): (Tree, List[Tree])
  def AppliedTypeTree_unapply(tree: AppliedTypeTree): (Tree, List[Tree])
  def MatchTypeTree_unapply(tree: MatchTypeTree): (Tree, Tree, List[CaseDef])
  def ByNameTypeTree_unapply(tree: ByNameTypeTree): Some[Tree]
  def Annotated_unapply(tree: Annotated): (Tree, Tree)
  def LambdaTypeTree_unapply(tree: LambdaTypeTree): (List[TypeDef], Tree)
  def TypeBoundsTree_unapply(tree: TypeBoundsTree): (Tree, Tree)
  def Hole_unapply(tree: Hole): (Int, List[Tree])
  def Thicket_unapply(tree: Thicket): Some[List[Tree]]

  def ValOrDefDef_name(tree: ValOrDefDef): TermName
  def ValOrDefDef_tpt(tree: ValOrDefDef): Tree
  def ValOrDefDef_rhs(tree: ValOrDefDef)(given Context): Tree
  def DefDef_tparams(tree: DefDef): List[TypeDef]
  def DefDef_vparamss(tree: DefDef): List[List[ValDef]]
  def TypeDef_rhs(tree: TypeDef): Tree

  def ImportSelector_imported(tree: untpd_ImportSelector): Ident
  def ImportSelector_renamed(tree: untpd_ImportSelector): Tree
  def ImportSelector_bound(tree: untpd_ImportSelector): untpd_Tree

  def Template_decomposeBody(tree: Template)(given Context): (List[Tree], List[Tree])
  def Template_parents(tree: Template): List[Tree]
  def Template_self(tree: Template): ValDef
  def Template_body(tree: Template): List[Tree]
  def Template_derived(tree: Template): List[untpd_Tree]
  def Template_constr(tree: Template): DefDef

  def Type_stripTypeVar(tpe: Type)(given Context): Type
  def Type_member(tpe: Type, name: Name)(given Context): Symbol // Denotation in dotty
  def Type_signature(tpe: Type)(given Context): Signature
  def Type_isContextualMethod(tpe: Type): Boolean
  def Type_isImplicitMethod(tpe: Type): Boolean
  def Type_isErasedMethod(tpe: Type): Boolean
  def Type_exists(tpe: Type): Boolean

  def AppliedType_unapply(tpe: AppliedType): (Type, List[Type])

  def ConstantType_value(tpe: ConstantType): Constant

  def ThisType_cls(tpe: ThisType)(given Context): ClassSymbol
  def ThisType_tref(tpe: ThisType): TypeRef

  def SuperType_thistpe(tpe: SuperType): Type
  def SuperType_supertpe(tpe: SuperType): Type

  def BoundType_binder(tpe: BoundType): tpe.BT

  def ParamRef_paramNum(tpe: ParamRef): Int

  def RecType_parent(tpe: RecType): Type

  def RefinedType_parent(tpe: RefinedType): Type
  def RefinedType_refinedName(tpe: RefinedType): Name
  def RefinedType_refinedInfo(tpe: RefinedType): Type

  def SkolemType_info(tpe: SkolemType): Type

  def NamedType_symbol(tpe: NamedType)(given Context): Symbol
  def NamedType_prefix(tpe: NamedType): Type
  def NamedType_designator(tpe: NamedType): Designator
  def NamedType_hasNoPrefix(tpe: Type): Boolean
  def NamedType_isType(tpe: NamedType): Boolean

  def TypeAlias_alias(tpe: TypeAlias): Type

  def TypeBounds_hi(tpe: TypeBounds): Type
  def TypeBounds_lo(tpe: TypeBounds): Type

  def AnnotatedType_parent(tpe: AnnotatedType): Type
  def AnnotatedType_annot(tpe: AnnotatedType): Annotation

  def Annotation_tree(annot: Annotation)(given Context): Tree
  def Annotation_symbol(annot: Annotation)(given Context): Symbol
  def Annotation_Child_unapply(annot: Annotation)(given Context): Option[Symbol]

  def AndOrType_tp1(tpe: AndOrType): Type
  def AndOrType_tp2(tpe: AndOrType): Type

  def TypeProxy_underlying(tpe: TypeProxy)(given Context): Type

  def LambdaType_resultType(tpe: LambdaType)(given Context): Type
  def LambdaType_paramNames(tpe: LambdaType): List[tpe.ThisName]
  def LambdaType_paramInfos(tpe: LambdaType): List[tpe.PInfo]

  def MatchType_bound(tpe: MatchType): Type
  def MatchType_scrutinee(tpe: MatchType): Type
  def MatchType_cases(tpe: MatchType): List[Type]

  def ClassInfo_selfInfo(tpe: ClassInfo): Either[Type, Symbol]

  def LazyRef_ref(tpe: LazyRef)(given Context): Type

  def TermName_tag(name: TermName): Int

  def SimpleName_toUTF8(name: SimpleName): Array[Byte]

  def String_toTermName(name: String): TermName

  def SignedName_unapply(name: DerivedName): Option[(TermName, Signature)]
  def SignedName_apply(name: TermName, sig: Signature): TermName

  def AnyQualifiedName_unapply(name: DerivedName): Option[(TermName, SimpleName)]
  def AnyUniqueName_unapply(name: DerivedName): Option[(TermName, String, Int)]
  def AnyNumberedName_unapply(name: DerivedName): Option[(TermName, Int)]
  def OuterSelectName_unapply(name: DerivedName): Option[(TermName, Int)]
  def DerivedName_unapply(name: DerivedName): Some[TermName]

  val nme_WILDCARD: TermName

  def Signature_ParamSig_fold[A](paramSig: Signature_ParamSig)(onInt: Int => A, onTypeName: TypeName => A): A
  def Signature_ParamSig_foldInt(paramSig: Signature_ParamSig)(onInt: IntToInt, onTypeName: ToInt[TypeName]): Int
  def Signature_isNotAMethod(sig: Signature): Boolean

  def Signature_unapply(signature: Signature): (List[Signature_ParamSig], TypeName)

  def pickling_println(msg: => String): Unit

  def StringContext_i(stringContext: StringContext, args: Any*): String

  def Comment_raw(comment: Comment): String
  def Comment_span(comment: Comment): Span
