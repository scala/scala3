package dotty.tools.dotc.core.tasty.experimental

import reflect.{classTag, ClassTag}

import dotty.tools.dotc.core.tasty.TreePickler

import dotty.tools.tasty.experimental.function._
import dotty.tools.tasty.experimental.bridge.TastyKernel
import dotty.tools.dotc.config.Printers
import dotty.tools.dotc.{util, core, ast, transform}

import core.Contexts, core.Names, core.Annotations, core.Types, core.Symbols, core.Flags, util.Spans,
  core.Comments, core.Constants, ast.Trees, core.Decorators, core.NameKinds, core.StdNames
import ast.{tpd, untpd}

import annotation.internal.sharable

object DottyKernel extends TastyKernel {

  type Context = Contexts.Context

  type SourceFile = util.SourceFile

  type Designator = Names.Designator

  type Annotation = Annotations.Annotation

  val Annotation_CT = classTag[Annotation]

  type Name = Names.Name
  type SimpleName = Names.SimpleName
  type DerivedName = Names.DerivedName
  type TypeName = Names.TypeName
  type TermName = Names.TermName

  val Name_CT = classTag[Name]
  val SimpleName_CT = classTag[SimpleName]
  val DerivedName_CT = classTag[DerivedName]
  val TypeName_CT = classTag[TypeName]
  val TermName_CT = classTag[TermName]

  type Signature = core.Signature
  val Signature_CT = classTag[Signature]

  type Positioned = ast.Positioned

  type untpd_Tree = untpd.Tree
  type untpd_ImportSelector = untpd.ImportSelector
  type untpd_TypedSplice = untpd.TypedSplice
  type untpd_MemberDef = untpd.MemberDef
  type untpd_Ident = untpd.Ident

  type Tree = tpd.Tree
  type MemberDef = tpd.MemberDef
  type Hole = TreePickler.Hole
  type Template = tpd.Template
  type ValOrDefDef = tpd.ValOrDefDef
  type TypeDef = tpd.TypeDef
  type ValDef = tpd.ValDef
  type DefDef = tpd.DefDef
  type RefTree = tpd.RefTree
  type Ident = tpd.Ident
  type This = tpd.This
  type Select = tpd.Select
  type Apply = tpd.Apply
  type TypeApply = tpd.TypeApply
  type Literal = tpd.Literal
  type Super = tpd.Super
  type New = tpd.New
  type Typed = tpd.Typed
  type NamedArg = tpd.NamedArg
  type Assign = tpd.Assign
  type Block = tpd.Block
  type If = tpd.If
  type Closure = tpd.Closure
  type Match = tpd.Match
  type CaseDef = tpd.CaseDef
  type Labeled = tpd.Labeled
  type Return = tpd.Return
  type WhileDo = tpd.WhileDo
  type Try = tpd.Try
  type SeqLiteral = tpd.SeqLiteral
  type Inlined = tpd.Inlined
  type Bind = tpd.Bind
  type Alternative = tpd.Alternative
  type UnApply = tpd.UnApply
  type Import = tpd.Import
  type PackageDef = tpd.PackageDef
  type TypeTree = tpd.TypeTree
  type SingletonTypeTree = tpd.SingletonTypeTree
  type RefinedTypeTree = tpd.RefinedTypeTree
  type AppliedTypeTree = tpd.AppliedTypeTree
  type MatchTypeTree = tpd.MatchTypeTree
  type ByNameTypeTree = tpd.ByNameTypeTree
  type Annotated = tpd.Annotated
  type LambdaTypeTree = tpd.LambdaTypeTree
  type TypeBoundsTree = tpd.TypeBoundsTree
  type Thicket = tpd.Thicket

  val untpd_Tree_CT: ClassTag[untpd_Tree] = new {
    def runtimeClass: Class[?] = classOf[untpd_Tree]
    override def unapply(x: Any): Option[untpd_Tree] = x match
      case tree: untpd.Tree => Some(tree)
      case _ => None
  }
  val untpd_TypedSplice_CT: ClassTag[untpd_TypedSplice] = new {
    def runtimeClass: Class[?] = classOf[untpd_TypedSplice]
    override def unapply(x: Any): Option[untpd_TypedSplice] = x match
      case tree: untpd.TypedSplice => Some(tree)
      case _ => None
  }
  val untpd_MemberDef_CT: ClassTag[untpd_MemberDef] = new {
    def runtimeClass: Class[?] = classOf[untpd_MemberDef]
    override def unapply(x: Any): Option[untpd_MemberDef] = x match
      case tree: untpd.MemberDef => Some(tree)
      case _ => None
  }
  val untpd_Ident_CT: ClassTag[untpd_Ident] = new {
    def runtimeClass: Class[?] = classOf[untpd_Ident]
    override def unapply(x: Any): Option[untpd_Ident] = x match
      case tree: untpd.Ident => Some(tree)
      case _ => None
  }

  val Tree_CT: ClassTag[Tree] = new {
    def runtimeClass: Class[?] = classOf[Tree]
    override def unapply(x: Any): Option[Tree] = x match
      case tree: tpd.Tree => Some(tree)
      case _ => None
  }
  val MemberDef_CT: ClassTag[MemberDef] = new {
    def runtimeClass: Class[?] = classOf[MemberDef]
    override def unapply(x: Any): Option[MemberDef] = x match
      case tree: tpd.MemberDef => Some(tree)
      case _ => None
  }
  val Hole_CT: ClassTag[Hole] = new {
    def runtimeClass: Class[?] = classOf[Hole]
    override def unapply(x: Any): Option[Hole] = x match
      case tree: TreePickler.Hole => Some(tree)
      case _ => None
  }
  val Template_CT: ClassTag[Template] = new {
    def runtimeClass: Class[?] = classOf[Template]
    override def unapply(x: Any): Option[Template] = x match
      case tree: tpd.Template => Some(tree)
      case _ => None
  }
  val ValOrDefDef_CT: ClassTag[ValOrDefDef] = new {
    def runtimeClass: Class[?] = classOf[ValOrDefDef]
    override def unapply(x: Any): Option[ValOrDefDef] = x match
      case tree: tpd.ValOrDefDef => Some(tree)
      case _ => None
  }
  val TypeDef_CT: ClassTag[TypeDef] = new {
    def runtimeClass: Class[?] = classOf[TypeDef]
    override def unapply(x: Any): Option[TypeDef] = x match
      case tree: tpd.TypeDef => Some(tree)
      case _ => None
  }
  val ValDef_CT: ClassTag[ValDef] = new {
    def runtimeClass: Class[?] = classOf[ValDef]
    override def unapply(x: Any): Option[ValDef] = x match
      case tree: tpd.ValDef => Some(tree)
      case _ => None
  }
  val DefDef_CT: ClassTag[DefDef] = new {
    def runtimeClass: Class[?] = classOf[DefDef]
    override def unapply(x: Any): Option[DefDef] = x match
      case tree: tpd.DefDef => Some(tree)
      case _ => None
  }
  val Ident_CT: ClassTag[Ident] = new {
    def runtimeClass: Class[?] = classOf[Ident]
    override def unapply(x: Any): Option[Ident] = x match
      case tree: tpd.Ident => Some(tree)
      case _ => None
  }
  val This_CT: ClassTag[This] = new {
    def runtimeClass: Class[?] = classOf[This]
    override def unapply(x: Any): Option[This] = x match
      case tree: tpd.This => Some(tree)
      case _ => None
  }
  val Select_CT: ClassTag[Select] = new {
    def runtimeClass: Class[?] = classOf[Select]
    override def unapply(x: Any): Option[Select] = x match
      case tree: tpd.Select => Some(tree)
      case _ => None
  }
  val Apply_CT: ClassTag[Apply] = new {
    def runtimeClass: Class[?] = classOf[Apply]
    override def unapply(x: Any): Option[Apply] = x match
      case tree: tpd.Apply => Some(tree)
      case _ => None
  }
  val TypeApply_CT: ClassTag[TypeApply] = new {
    def runtimeClass: Class[?] = classOf[TypeApply]
    override def unapply(x: Any): Option[TypeApply] = x match
      case tree: tpd.TypeApply => Some(tree)
      case _ => None
  }
  val Literal_CT: ClassTag[Literal] = new {
    def runtimeClass: Class[?] = classOf[Literal]
    override def unapply(x: Any): Option[Literal] = x match
      case tree: tpd.Literal => Some(tree)
      case _ => None
  }
  val Super_CT: ClassTag[Super] = new {
    def runtimeClass: Class[?] = classOf[Super]
    override def unapply(x: Any): Option[Super] = x match
      case tree: tpd.Super => Some(tree)
      case _ => None
  }
  val New_CT: ClassTag[New] = new {
    def runtimeClass: Class[?] = classOf[New]
    override def unapply(x: Any): Option[New] = x match
      case tree: tpd.New => Some(tree)
      case _ => None
  }
  val Typed_CT: ClassTag[Typed] = new {
    def runtimeClass: Class[?] = classOf[Typed]
    override def unapply(x: Any): Option[Typed] = x match
      case tree: tpd.Typed => Some(tree)
      case _ => None
  }
  val NamedArg_CT: ClassTag[NamedArg] = new {
    def runtimeClass: Class[?] = classOf[NamedArg]
    override def unapply(x: Any): Option[NamedArg] = x match
      case tree: tpd.NamedArg => Some(tree)
      case _ => None
  }
  val Assign_CT: ClassTag[Assign] = new {
    def runtimeClass: Class[?] = classOf[Assign]
    override def unapply(x: Any): Option[Assign] = x match
      case tree: tpd.Assign => Some(tree)
      case _ => None
  }
  val Block_CT: ClassTag[Block] = new {
    def runtimeClass: Class[?] = classOf[Block]
    override def unapply(x: Any): Option[Block] = x match
      case tree: tpd.Block => Some(tree)
      case _ => None
  }
  val If_CT: ClassTag[If] = new {
    def runtimeClass: Class[?] = classOf[If]
    override def unapply(x: Any): Option[If] = x match
      case tree: tpd.If => Some(tree)
      case _ => None
  }
  val Closure_CT: ClassTag[Closure] = new {
    def runtimeClass: Class[?] = classOf[Closure]
    override def unapply(x: Any): Option[Closure] = x match
      case tree: tpd.Closure => Some(tree)
      case _ => None
  }
  val Match_CT: ClassTag[Match] = new {
    def runtimeClass: Class[?] = classOf[Match]
    override def unapply(x: Any): Option[Match] = x match
      case tree: tpd.Match => Some(tree)
      case _ => None
  }
  val CaseDef_CT: ClassTag[CaseDef] = new {
    def runtimeClass: Class[?] = classOf[CaseDef]
    override def unapply(x: Any): Option[CaseDef] = x match
      case tree: tpd.CaseDef => Some(tree)
      case _ => None
  }
  val Labeled_CT: ClassTag[Labeled] = new {
    def runtimeClass: Class[?] = classOf[Labeled]
    override def unapply(x: Any): Option[Labeled] = x match
      case tree: tpd.Labeled => Some(tree)
      case _ => None
  }
  val Return_CT: ClassTag[Return] = new {
    def runtimeClass: Class[?] = classOf[Return]
    override def unapply(x: Any): Option[Return] = x match
      case tree: tpd.Return => Some(tree)
      case _ => None
  }
  val WhileDo_CT: ClassTag[WhileDo] = new {
    def runtimeClass: Class[?] = classOf[WhileDo]
    override def unapply(x: Any): Option[WhileDo] = x match
      case tree: tpd.WhileDo => Some(tree)
      case _ => None
  }
  val Try_CT: ClassTag[Try] = new {
    def runtimeClass: Class[?] = classOf[Try]
    override def unapply(x: Any): Option[Try] = x match
      case tree: tpd.Try => Some(tree)
      case _ => None
  }
  val SeqLiteral_CT: ClassTag[SeqLiteral] = new {
    def runtimeClass: Class[?] = classOf[SeqLiteral]
    override def unapply(x: Any): Option[SeqLiteral] = x match
      case tree: tpd.SeqLiteral => Some(tree)
      case _ => None
  }
  val Inlined_CT: ClassTag[Inlined] = new {
    def runtimeClass: Class[?] = classOf[Inlined]
    override def unapply(x: Any): Option[Inlined] = x match
      case tree: tpd.Inlined => Some(tree)
      case _ => None
  }
  val Bind_CT: ClassTag[Bind] = new {
    def runtimeClass: Class[?] = classOf[Bind]
    override def unapply(x: Any): Option[Bind] = x match
      case tree: tpd.Bind => Some(tree)
      case _ => None
  }
  val Alternative_CT: ClassTag[Alternative] = new {
    def runtimeClass: Class[?] = classOf[Alternative]
    override def unapply(x: Any): Option[Alternative] = x match
      case tree: tpd.Alternative => Some(tree)
      case _ => None
  }
  val UnApply_CT: ClassTag[UnApply] = new {
    def runtimeClass: Class[?] = classOf[UnApply]
    override def unapply(x: Any): Option[UnApply] = x match
      case tree: tpd.UnApply => Some(tree)
      case _ => None
  }
  val Import_CT: ClassTag[Import] = new {
    def runtimeClass: Class[?] = classOf[Import]
    override def unapply(x: Any): Option[Import] = x match
      case tree: tpd.Import => Some(tree)
      case _ => None
  }
  val PackageDef_CT: ClassTag[PackageDef] = new {
    def runtimeClass: Class[?] = classOf[PackageDef]
    override def unapply(x: Any): Option[PackageDef] = x match
      case tree: tpd.PackageDef => Some(tree)
      case _ => None
  }
  val TypeTree_CT: ClassTag[TypeTree] = new {
    def runtimeClass: Class[?] = classOf[TypeTree]
    override def unapply(x: Any): Option[TypeTree] = x match
      case tree: tpd.TypeTree => Some(tree)
      case _ => None
  }
  val SingletonTypeTree_CT: ClassTag[SingletonTypeTree] = new {
    def runtimeClass: Class[?] = classOf[SingletonTypeTree]
    override def unapply(x: Any): Option[SingletonTypeTree] = x match
      case tree: tpd.SingletonTypeTree => Some(tree)
      case _ => None
  }
  val RefinedTypeTree_CT: ClassTag[RefinedTypeTree] = new {
    def runtimeClass: Class[?] = classOf[RefinedTypeTree]
    override def unapply(x: Any): Option[RefinedTypeTree] = x match
      case tree: tpd.RefinedTypeTree => Some(tree)
      case _ => None
  }
  val AppliedTypeTree_CT: ClassTag[AppliedTypeTree] = new {
    def runtimeClass: Class[?] = classOf[AppliedTypeTree]
    override def unapply(x: Any): Option[AppliedTypeTree] = x match
      case tree: tpd.AppliedTypeTree => Some(tree)
      case _ => None
  }
  val MatchTypeTree_CT: ClassTag[MatchTypeTree] = new {
    def runtimeClass: Class[?] = classOf[MatchTypeTree]
    override def unapply(x: Any): Option[MatchTypeTree] = x match
      case tree: tpd.MatchTypeTree => Some(tree)
      case _ => None
  }
  val ByNameTypeTree_CT: ClassTag[ByNameTypeTree] = new {
    def runtimeClass: Class[?] = classOf[ByNameTypeTree]
    override def unapply(x: Any): Option[ByNameTypeTree] = x match
      case tree: tpd.ByNameTypeTree => Some(tree)
      case _ => None
  }
  val Annotated_CT: ClassTag[Annotated] = new {
    def runtimeClass: Class[?] = classOf[Annotated]
    override def unapply(x: Any): Option[Annotated] = x match
      case tree: tpd.Annotated => Some(tree)
      case _ => None
  }
  val LambdaTypeTree_CT: ClassTag[LambdaTypeTree] = new {
    def runtimeClass: Class[?] = classOf[LambdaTypeTree]
    override def unapply(x: Any): Option[LambdaTypeTree] = x match
      case tree: tpd.LambdaTypeTree => Some(tree)
      case _ => None
  }
  val TypeBoundsTree_CT: ClassTag[TypeBoundsTree] = new {
    def runtimeClass: Class[?] = classOf[TypeBoundsTree]
    override def unapply(x: Any): Option[TypeBoundsTree] = x match
      case tree: tpd.TypeBoundsTree => Some(tree)
      case _ => None
  }
  val Thicket_CT: ClassTag[Thicket] = new {
    def runtimeClass: Class[?] = classOf[Thicket]
    override def unapply(x: Any): Option[Thicket] = x match
      case tree: tpd.Thicket => Some(tree)
      case _ => None
  }

  type Type = Types.Type
  type AppliedType = Types.AppliedType
  type ConstantType = Types.ConstantType
  type ClassInfo = Types.ClassInfo
  type NamedType = Types.NamedType
  type ThisType = Types.ThisType
  type SuperType = Types.SuperType
  type BoundType = Types.BoundType
  type RecThis = Types.RecThis
  type ParamRef = Types.ParamRef
  type RecType = Types.RecType
  type RefinedType = Types.RefinedType
  type SkolemType = Types.SkolemType
  type TypeBounds = Types.TypeBounds
  type TypeAlias = Types.TypeAlias
  type TermRef = Types.TermRef
  type TypeRef = Types.TypeRef
  type AnnotatedType = Types.AnnotatedType
  type AndOrType = Types.AndOrType
  type AndType = Types.AndType
  type OrType = Types.OrType
  type TypeProxy = Types.TypeProxy
  type ExprType = Types.ExprType
  type MatchType = Types.MatchType
  type LambdaType = Types.LambdaType
  type HKTypeLambda = Types.HKTypeLambda
  type PolyType = Types.PolyType
  type MethodType = Types.MethodType
  type LazyRef = Types.LazyRef

  val Type_CT = classTag[Type]
  val AppliedType_CT = classTag[AppliedType]
  val ConstantType_CT = classTag[ConstantType]
  val NamedType_CT = classTag[NamedType]
  val ThisType_CT = classTag[ThisType]
  val SuperType_CT = classTag[SuperType]
  val RecThis_CT = classTag[RecThis]
  val RecType_CT = classTag[RecType]
  val TermRef_CT = classTag[TermRef]
  val TypeRef_CT = classTag[TypeRef]
  val ParamRef_CT = classTag[ParamRef]
  val SkolemType_CT = classTag[SkolemType]
  val RefinedType_CT = classTag[RefinedType]
  val TypeAlias_CT = classTag[TypeAlias]
  val TypeBounds_CT = classTag[TypeBounds]
  val AnnotatedType_CT = classTag[AnnotatedType]
  val AndType_CT = classTag[AndType]
  val OrType_CT = classTag[OrType]
  val MatchType_CT = classTag[MatchType]
  val ExprType_CT = classTag[ExprType]
  val HKTypeLambda_CT = classTag[HKTypeLambda]
  val PolyType_CT = classTag[PolyType]
  val MethodType_CT = classTag[MethodType]
  val LazyRef_CT = classTag[LazyRef]
  val ClassInfo_CT = classTag[ClassInfo]

  type Symbol = Symbols.Symbol
  type TermSymbol = Symbols.TermSymbol
  type TypeSymbol = Symbols.TypeSymbol
  type ClassSymbol = Symbols.ClassSymbol

  type FlagSet = Flags.FlagSet
  type Flag = Flags.Flag

  val Symbol_CT = classTag[Symbol]
  val ClassSymbol_CT = classTag[ClassSymbol]

  type Symbols_MutableSymbolMap[T] = Symbols.MutableSymbolMap[T]

  type SourcePosition = util.SourcePosition
  type Span = Spans.Span

  type ContextDocstrings = Comments.ContextDocstrings

  type Comment = Comments.Comment

  type Constant = Constants.Constant

  final val Flags_Protected = Flags.Protected
  final val Flags_ParamAccessor = Flags.ParamAccessor
  final val Flags_Private = Flags.Private
  final val Flags_Final = Flags.Final
  final val Flags_Case = Flags.Case
  final val Flags_Override = Flags.Override
  final val Flags_Inline = Flags.Inline
  final val Flags_InlineProxy = Flags.InlineProxy
  final val Flags_Macro = Flags.Macro
  final val Flags_JavaStatic = Flags.JavaStatic
  final val Flags_Module = Flags.Module
  final val Flags_Enum = Flags.Enum
  final val Flags_Local = Flags.Local
  final val Flags_Synthetic = Flags.Synthetic
  final val Flags_Artifact = Flags.Artifact
  final val Flags_Scala2x = Flags.Scala2x
  final val Flags_Implicit = Flags.Implicit
  final val Flags_Given = Flags.Given
  final val Flags_Erased = Flags.Erased
  final val Flags_Lazy = Flags.Lazy
  final val Flags_AbsOverride = Flags.AbsOverride
  final val Flags_Mutable = Flags.Mutable
  final val Flags_Accessor = Flags.Accessor
  final val Flags_CaseAccessor = Flags.CaseAccessor
  final val Flags_DefaultParameterized = Flags.DefaultParameterized
  final val Flags_StableRealizable = Flags.StableRealizable
  final val Flags_Extension = Flags.Extension
  final val Flags_Exported = Flags.Exported
  final val Flags_Label = Flags.Label
  final val Flags_Sealed = Flags.Sealed
  final val Flags_Abstract = Flags.Abstract
  final val Flags_Trait = Flags.Trait
  final val Flags_Covariant = Flags.Covariant
  final val Flags_Contravariant = Flags.Contravariant
  final val Flags_Opaque = Flags.Opaque
  final val Flags_Open = Flags.Open

  def FlagSet_is(flags: FlagSet, flag: Flag): Boolean = flags.is(flag)
  def FlagSet_is(flags: FlagSet, flag: Flag, butNot: FlagSet): Boolean = flags.is(flag, butNot)
  def FlagSet_&~(flags: FlagSet, flag: Flag): FlagSet = flags &~ flag

  def Context_log(ctx: Context, msg: => String, sourcePos: SourcePosition): Unit = ctx.log(msg, sourcePos)
  def Context_source(ctx: Context): SourceFile = ctx.source
  def Context_docCtx(ctx: Context): Option[ContextDocstrings] = {
    import Comments._
    ctx.docCtx
  }
  def Context_withOwner(ctx: Context, owner: Symbol): Context = ctx.withOwner(owner)
  def Context_withSource(ctx: Context, source: SourceFile): Context = ctx.withSource(source)

  def ContextDocstrings_docstring(ctx: ContextDocstrings, sym: Symbol): Option[Comment] = ctx.docstring(sym)

  def Constant_tag(c: Constant): Int = c.tag
  def Constant_intValue(c: Constant): Int = c.intValue
  def Constant_booleanValue(c: Constant): Boolean = c.booleanValue
  def Constant_byteValue(c: Constant): Byte = c.byteValue
  def Constant_charValue(c: Constant): Char = c.charValue
  def Constant_shortValue(c: Constant): Short = c.shortValue
  def Constant_longValue(c: Constant): Long = c.longValue
  def Constant_doubleValue(c: Constant): Double = c.doubleValue
  def Constant_floatValue(c: Constant): Float = c.floatValue
  def Constant_stringValue(c: Constant): String = c.stringValue
  def Constant_typeValue(c: Constant): Type = c.typeValue
  def Constant_symbolValue(c: Constant): Symbol = c.symbolValue

  def Symbols_MutableSymbolMap_get[T](map: Symbols_MutableSymbolMap[T], sym: Symbol): Option[T] = map.get(sym)
  def Symbols_MutableSymbolMap_getOrElse[U >: T, T](map: Symbols_MutableSymbolMap[T], sym: Symbol, default: => U): U = map.getOrElse(sym, default)
  def Symbols_MutableSymbolMap_contains[T](map: Symbols_MutableSymbolMap[T], sym: Symbol): Boolean = map.contains(sym)
  def Symbols_MutableSymbolMap_update[T](map: Symbols_MutableSymbolMap[T], sym: Symbol, value: T): Unit = map.update(sym, value)
  def Symbols_MutableSymbolMap_-=[T](map: Symbols_MutableSymbolMap[T], sym: Symbol): Unit = map -= sym
  def Symbols_MutableSymbolMap_apply[T](map: Symbols_MutableSymbolMap[T], sym: Symbol): T = map(sym)
  def Symbols_MutableSymbolMap_keysIterator[T](map: Symbols_MutableSymbolMap[T]): Iterator[Symbol] = map.keysIterator
  def Symbols_MutableSymbolMap_isEmpty[T](map: Symbols_MutableSymbolMap[T]): Boolean = map.isEmpty
  def Symbols_newMutableSymbolMap[A]: Symbols_MutableSymbolMap[A] = Symbols.newMutableSymbolMap

  def Symbol_isPackage(sym: Symbol)(given Context): Boolean = sym.is(Flags.Package)
  def Symbol_isPrivate(sym: Symbol)(given Context): Boolean = sym.is(Flags.Private)
  def Symbol_sourcePos(sym: Symbol)(given Context): SourcePosition = sym.sourcePos
  def Symbol_owner(sym: Symbol)(given Context): Symbol = sym.owner
  def Symbol_isDefinedWithin(sym: Symbol, outer: Symbol)(given Context): Boolean = sym.topLevelClass.isLinkedWith(outer)
  def Symbol_termRef(sym: Symbol)(given Context): TermRef = sym.termRef
  def Symbol_typeRef(sym: Symbol)(given Context): TypeRef = sym.typeRef
  def Symbol_name(sym: Symbol)(given Context): sym.ThisName = sym.name
  def Symbol_fullName(sym: Symbol)(given Context): Name = sym.fullName
  def Symbol_isClass(sym: Symbol): Boolean = sym.isClass
  def Symbol_exists(sym: Symbol)(given Context): Boolean = sym.exists
  def Symbol_isEffectiveRoot(sym: Symbol)(given Context): Boolean = sym.isEffectiveRoot
  def Symbol_flags(sym: Symbol)(given Context): FlagSet = sym.flags
  def Symbol_privateWithin(sym: Symbol)(given Context): Symbol = sym.privateWithin
  def Symbol_isTerm(sym: Symbol)(given Context): Boolean = sym.isTerm
  def Symbol_isSetter(sym: Symbol)(given Context): Boolean = sym.isSetter
  def Symbol_info(sym: Symbol)(given Context): Type = sym.info
  def Symbol_showLocated(sym: Symbol)(given Context): String = sym.showLocated
  def Symbol_annotations(sym: Symbol)(given Context): List[Annotation] = sym.annotations
  def Symbol_isInaccessibleChildOf(sym: Symbol, cls: Symbol)(given Context): Boolean = {
    import transform.SymUtils._
    sym.isInaccessibleChildOf(cls)
  }

  def SourceFile_path(source: SourceFile): String = source.path
  def SourceFile_exists(source: SourceFile): Boolean = source.exists
  @sharable val SourceFile_noSource: SourceFile = util.NoSource

  def SourcePosition_line(pos: SourcePosition): Int = pos.line

  @sharable val Span_empty: Span = Spans.Span(0,0)
  @sharable val Span_noSpan: Span = Spans.NoSpan
  def Span_start(span: Span): Int = span.start
  def Span_end(span: Span): Int = span.end
  def Span_isSynthetic(span: Span): Boolean = span.isSynthetic
  def Span_toSynthetic(span: Span): Span = span.toSynthetic
  def Span_pointDelta(span: Span): Int = span.pointDelta
  def Span_coords(span: Span): Long = span.coords
  def Span_exists(span: Span): Boolean = span.exists

  private inline def defn(given ctx: Context) = ctx.definitions

  def defn_throwMethod(given ctx: Context): TermSymbol = defn.throwMethod
  def defn_BodyAnnot(given Context): ClassSymbol = defn.BodyAnnot

  def Name_toTermName(name: Name): TermName = name.toTermName
  def Name_isEmpty(name: Name): Boolean = name.isEmpty
  def Name_isTypeName(name: Name): Boolean = name.isTypeName

  def Positioned_alwaysNeedsPos(positioned: Positioned): Boolean = positioned match {
    case
        // initialSpan is inaccurate for trees with lazy field
        _: Trees.WithLazyField[?]

        // A symbol is created before the corresponding tree is unpickled,
        // and its position cannot be changed afterwards.
        // so we cannot use the tree initialSpan to set the symbol position.
        // Instead, we always pickle the position of definitions.
        | _: Trees.DefTree[?]

        // package defs might be split into several Tasty files
        | _: Trees.PackageDef[?]
        // holes can change source files when filled, which means
        // they might lose their position
        | _: TreePickler.Hole => true
    case _ => false
  }

  def untpd_Tree_span(tree: untpd_Tree): Span = tree.span
  def untpd_Tree_source(tree: untpd_Tree): SourceFile = tree.source
  def untpd_Tree_envelope(tree: untpd_Tree, src: SourceFile, startSpan: Span): Span = tree.envelope(src, startSpan)
  def untpd_Tree_symbol(tree: untpd_Tree)(given Context): Symbol = tree.symbol
  def untpd_Tree_withType(tree: untpd_Tree, tpe: Type)(given Context): tree.ThisTree[Type] = tree.withType(tpe)
  def untpd_Tree_isEmpty(tree: untpd_Tree): Boolean = tree.isEmpty

  def Tree_isType(tree: Tree): Boolean = tree.isType
  def Tree_tpe(tree: Tree): Type = tree.tpe
  @sharable val EmptyTree: Tree = tpd.EmptyTree

  def If_isInline(tree: If): Boolean = tree.isInline
  def Match_isInline(tree: Match): Boolean = tree.isInline

  def inlineContext(tree: Tree)(implicit ctx: Context): Context = tpd.inlineContext(tree)

  def untpd_TypedSplice_unapply(tree: untpd_TypedSplice): Some[Tree] = Some(tree.splice)
  def untpd_Ident_unapply(tree: untpd_Ident): Some[Name] = Some(tree.name)
  def Ident_unapply(tree: Ident): Some[Name] = Some(tree.name)
  def This_unapply(tree: This): Some[untpd_Ident] = Some(tree.qual)
  def Select_unapply(tree: Select): (Tree, Name) = (tree.qualifier, tree.name)
  def Apply_unapply(tree: Apply): (Tree, List[Tree]) = (tree.fun, tree.args)
  def TypeApply_unapply(tree: TypeApply): (Tree, List[Tree]) = (tree.fun, tree.args)
  def Literal_unapply(tree: Literal): Some[Constant] = Some(tree.const)
  def Super_unapply(tree: Super): (Tree, untpd_Ident) = (tree.qual, tree.mix)
  def New_unapply(tree: New): Some[Tree] = Some(tree.tpt)
  def Typed_unapply(tree: Typed): (Tree, Tree) = (tree.expr, tree.tpt)
  def NamedArg_unapply(tree: NamedArg): (Name, Tree) = (tree.name, tree.arg)
  def Assign_unapply(tree: Assign): (Tree, Tree) = (tree.lhs, tree.rhs)
  def Block_unapply(tree: Block): (List[Tree], Tree) = (tree.stats, tree.expr)
  def If_unapply(tree: If): (Tree, Tree, Tree) = (tree.cond, tree.thenp, tree.elsep)
  def Closure_unapply(tree: Closure): (List[Tree], Tree, Tree) = (tree.env, tree.meth, tree.tpt)
  def Match_unapply(tree: Match): (Tree, List[CaseDef]) = (tree.selector, tree.cases)
  def CaseDef_unapply(tree: CaseDef): (Tree, Tree, Tree) = (tree.pat, tree.guard, tree.body)
  def Labeled_unapply(tree: Labeled): (Bind, Tree) = (tree.bind, tree.expr)
  def Return_unapply(tree: Return): (Tree, Tree) = (tree.expr, tree.from)
  def WhileDo_unapply(tree: WhileDo): (Tree, Tree) = (tree.cond, tree.body)
  def Try_unapply(tree: Try): (Tree, List[CaseDef], Tree) = (tree.expr, tree.cases, tree.finalizer)
  def SeqLiteral_unapply(tree: SeqLiteral): (List[Tree], Tree) = (tree.elems, tree.elemtpt)
  def Inlined_unapply(tree: Inlined): (Tree, List[MemberDef], Tree) = (tree.call, tree.bindings, tree.expansion)
  def Bind_unapply(tree: Bind): (Name, Tree) = (tree.name, tree.body)
  def Alternative_unapply(tree: Alternative): Some[List[Tree]] = Some(tree.trees)
  def UnApply_unapply(tree: UnApply): (Tree, List[Tree], List[Tree]) = (tree.fun, tree.implicits, tree.patterns)
  def Import_unapply(tree: Import): (Tree, List[untpd_ImportSelector]) = (tree.expr, tree.selectors)
  def PackageDef_unapply(tree: PackageDef): (RefTree, List[Tree]) = (tree.pid, tree.stats)
  def SingletonTypeTree_unapply(tree: SingletonTypeTree): Some[Tree] = Some(tree.ref)
  def RefinedTypeTree_unapply(tree: RefinedTypeTree): (Tree, List[Tree]) = (tree.tpt, tree.refinements)
  def AppliedTypeTree_unapply(tree: AppliedTypeTree): (Tree, List[Tree]) = (tree.tpt, tree.args)
  def MatchTypeTree_unapply(tree: MatchTypeTree): (Tree, Tree, List[CaseDef]) = (tree.bound, tree.selector, tree.cases)
  def ByNameTypeTree_unapply(tree: ByNameTypeTree): Some[Tree] = Some(tree.result)
  def Annotated_unapply(tree: Annotated): (Tree, Tree) = (tree.arg, tree.annot)
  def LambdaTypeTree_unapply(tree: LambdaTypeTree): (List[TypeDef], Tree) = (tree.tparams, tree.body)
  def TypeBoundsTree_unapply(tree: TypeBoundsTree): (Tree, Tree) = (tree.lo, tree.hi)
  def Hole_unapply(tree: Hole): (Int, List[Tree]) = (tree.idx, tree.args)
  def Thicket_unapply(tree: Thicket): Some[List[Tree]] = Some(tree.trees)

  def ValOrDefDef_name(tree: ValOrDefDef): TermName = tree.name
  def ValOrDefDef_tpt(tree: ValOrDefDef): Tree = tree.tpt
  def ValOrDefDef_rhs(tree: ValOrDefDef)(given Context): Tree = tree.rhs
  def DefDef_tparams(tree: DefDef): List[TypeDef] = tree.tparams
  def DefDef_vparamss(tree: DefDef): List[List[ValDef]] = tree.vparamss
  def TypeDef_rhs(tree: TypeDef): Tree = tree.rhs

  def ImportSelector_imported(tree: untpd_ImportSelector): untpd.Ident = tree.imported
  def ImportSelector_renamed(tree: untpd_ImportSelector): untpd.Tree = tree.renamed
  def ImportSelector_bound(tree: untpd_ImportSelector): untpd_Tree = tree.bound

  def Template_decomposeBody(tree: Template)(given Context): (List[Tree], List[Tree]) =
    tpd.decomposeTemplateBody(tree.body)
  def Template_parents(tree: Template): List[Tree] = tree.parents
  def Template_self(tree: Template): ValDef = tree.self
  def Template_body(tree: Template)(given Context): List[Tree] = tree.body
  def Template_derived(tree: Template): List[untpd_Tree] = tree.derived
  def Template_constr(tree: Template): DefDef = tree.constr

  def Type_stripTypeVar(tpe: Type)(given Context): Type = tpe.stripTypeVar
  def Type_member(tpe: Type, name: Name)(given Context): Symbol = tpe.member(name).symbol
  def Type_signature(tpe: Type)(given Context): Signature = tpe.signature
  def Type_isContextualMethod(tpe: Type): Boolean = tpe.isContextualMethod
  def Type_isImplicitMethod(tpe: Type): Boolean = tpe.isImplicitMethod
  def Type_isErasedMethod(tpe: Type): Boolean = tpe.isErasedMethod
  def Type_exists(tpe: Type): Boolean = tpe.exists

  def AppliedType_unapply(tpe: AppliedType): (Type, List[Type]) = (tpe.tycon, tpe.args)

  def ConstantType_value(tpe: ConstantType): Constant = tpe.value

  def ThisType_cls(tpe: ThisType)(given Context): ClassSymbol = tpe.cls
  def ThisType_tref(tpe: ThisType): TypeRef = tpe.tref

  def SuperType_thistpe(tpe: SuperType): Type = tpe.thistpe
  def SuperType_supertpe(tpe: SuperType): Type = tpe.supertpe

  def BoundType_binder(tpe: BoundType): tpe.BT = tpe.binder

  def ParamRef_paramNum(tpe: ParamRef): Int = tpe.paramNum

  def RecType_parent(tpe: RecType): Type = tpe.parent

  def RefinedType_parent(tpe: RefinedType): Type = tpe.parent
  def RefinedType_refinedName(tpe: RefinedType): Name = tpe.refinedName
  def RefinedType_refinedInfo(tpe: RefinedType): Type = tpe.refinedInfo

  def SkolemType_info(tpe: SkolemType): Type = tpe.info

  def NamedType_symbol(tpe: NamedType)(given Context): Symbol = tpe.symbol
  def NamedType_prefix(tpe: NamedType): Type = tpe.prefix
  def NamedType_designator(tpe: NamedType): Designator = tpe.designator
  def NamedType_hasNoPrefix(tpe: NamedType): Boolean = tpe.prefix `eq` Types.NoPrefix
  def NamedType_isType(tpe: NamedType): Boolean = tpe.isType

  def TypeAlias_alias(tpe: TypeAlias): Type = tpe.alias

  def TypeBounds_hi(tpe: TypeBounds): Type = tpe.hi
  def TypeBounds_lo(tpe: TypeBounds): Type = tpe.lo

  def AnnotatedType_parent(tpe: AnnotatedType): Type = tpe.parent
  def AnnotatedType_annot(tpe: AnnotatedType): Annotation = tpe.annot

  def Annotation_tree(annot: Annotation)(given Context): Tree = annot.tree
  def Annotation_symbol(annot: Annotation)(given Context): Symbol = annot.symbol
  def Annotation_Child_unapply(annot: Annotation)(given Context): Option[Symbol] =
    Annotations.Annotation.Child.unapply(annot)

  def AndOrType_tp1(tpe: AndOrType): Type = tpe.tp1
  def AndOrType_tp2(tpe: AndOrType): Type = tpe.tp2

  def TypeProxy_underlying(tpe: TypeProxy)(given Context): Type = tpe.underlying

  def LambdaType_resultType(tpe: LambdaType)(given Context): Type = tpe.resultType
  def LambdaType_paramNames(tpe: LambdaType): List[tpe.ThisName] = tpe.paramNames
  def LambdaType_paramInfos(tpe: LambdaType): List[tpe.PInfo] = tpe.paramInfos

  def MatchType_bound(tpe: MatchType): Type = tpe.bound
  def MatchType_scrutinee(tpe: MatchType): Type = tpe.scrutinee
  def MatchType_cases(tpe: MatchType): List[Type] = tpe.cases

  def ClassInfo_selfInfo(tpe: ClassInfo): Either[Type, Symbol] = tpe.selfInfo match {
    case tp: Type => Left(tp)
    case sym: Symbol => Right(sym)
  }

  def LazyRef_ref(tpe: LazyRef)(given Context): Type = tpe.ref

  def TermName_tag(name: TermName): Int = name.info.kind.tag

  def SimpleName_toUTF8(name: SimpleName): Array[Byte] =
    if (name.length == 0) new Array[Byte](0)
    else io.Codec.toUTF8(Names.chrs, name.start, name.length)

  def String_toTermName(name: String): TermName = {
    import Decorators._
    name.toTermName
  }

  def SignedName_unapply(name: DerivedName): Option[(TermName, Signature)] = NameKinds.SignedName.unapply(name)
  def SignedName_apply(name: TermName, sig: Signature): TermName = NameKinds.SignedName.apply(name, sig)

  def AnyQualifiedName_unapply(name: DerivedName): Option[(TermName, SimpleName)] = NameKinds.AnyQualifiedName.unapply(name)
  def AnyUniqueName_unapply(name: DerivedName): Option[(TermName, String, Int)] = NameKinds.AnyUniqueName.unapply(name)
  def AnyNumberedName_unapply(name: DerivedName): Option[(TermName, Int)] = NameKinds.AnyNumberedName.unapply(name)
  def OuterSelectName_unapply(name: DerivedName): Option[(TermName, Int)] = NameKinds.OuterSelectName.unapply(name)
  def DerivedName_unapply(name: DerivedName): Some[TermName] = Some(name.underlying)

  @sharable val nme_WILDCARD: TermName = StdNames.nme.WILDCARD

  def Signature_ParamSig_fold[A](paramSig: Signature_ParamSig)(onInt: Int => A, onTypeName: TypeName => A): A =
    paramSig match {
      case i: Int => onInt(i)
      case n: TypeName => onTypeName(n)
    }

  def Signature_ParamSig_foldInt(paramSig: Signature_ParamSig)(onInt: IntToInt, onTypeName: ToInt[TypeName]): Int =
    paramSig match {
      case i: Int => onInt(i)
      case n: TypeName => onTypeName(n)
    }

  def Signature_isNotAMethod(sig: Signature): Boolean = sig `eq` core.Signature.NotAMethod

  def Signature_unapply(signature: Signature): (List[Signature_ParamSig], TypeName) =
    (signature.paramsSig, signature.resSig)

  def pickling_println(msg: => String): Unit = Printers.pickling.println(msg)

  def StringContext_i(stringContext: StringContext, args: Any*)(given Context): String = {
    import Decorators._
    stringContext.i(args)
  }

  def Comment_raw(comment: Comment): String = comment.raw
  def Comment_span(comment: Comment): Span = comment.span

}
