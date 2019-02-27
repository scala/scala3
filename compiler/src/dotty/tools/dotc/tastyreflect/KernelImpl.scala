package dotty.tools.dotc
package tastyreflect

import dotty.tools.dotc.ast.{Trees, tpd, untpd}
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.{Constants, NameKinds, Types}
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.quoted.PickledQuotes
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.tastyreflect.FromSymbol.{definitionFromSym, packageDefFromSym}

import scala.tasty.reflect.Kernel

class KernelImpl extends Kernel {

  //
  // CONTEXT
  //

  type Context = core.Contexts.Context

  def Context_owner(self: Context): Symbol = self.owner

  def Context_source(self: Context): java.nio.file.Path = self.compilationUnit.source.file.jpath

  //
  // Settings
  //

  type Settings = config.ScalaSettings

  //
  // TREES
  //

  type TermOrTypeTree = tpd.Tree

  type Tree = tpd.Tree

  def Tree_pos(self: Tree)(implicit ctx: Context): Position = self.sourcePos
  def Tree_symbol(self: Tree)(implicit ctx: Context): Symbol = self.symbol

  type PackageClause = tpd.PackageDef

  def PackageClause_pid(self: PackageClause)(implicit ctx: Context): Term_Ref = self.pid
  def PackageClause_stats(self: PackageClause)(implicit ctx: Context): List[Tree] = self.stats

  type Statement = tpd.Tree

  type Import = tpd.Import

  def Import_impliedOnly(self: Import): Boolean = self.impliedOnly
  def Import_expr(self: Import)(implicit ctx: Context): Tree = self.expr
  def Import_selectors(self: Import)(implicit ctx: Context): List[ImportSelector] = self.selectors

  type Definition = tpd.Tree

  def Definition_name(self: Definition)(implicit ctx: Context): String = self.symbol.name.toString

  type PackageDef = PackageDefinition

  def PackageDef_owner(self: PackageDef)(implicit ctx: Context): PackageDef = packageDefFromSym(self.symbol.owner)

  def PackageDef_members(self: PackageDef)(implicit ctx: Context): List[Statement] = {
    if (self.symbol.is(core.Flags.JavaDefined)) Nil // FIXME should also support java packages
    else self.symbol.info.decls.iterator.map(definitionFromSym).toList
  }

  def PackageDef_symbol(self: PackageDef)(implicit ctx: Context): PackageSymbol = self.symbol

  type ClassDef = tpd.TypeDef

  def ClassDef_constructor(self: ClassDef)(implicit ctx: Context): DefDef = ClassDef_rhs(self).constr
  def ClassDef_parents(self: ClassDef)(implicit ctx: Context): List[TermOrTypeTree] = ClassDef_rhs(self).parents
  def ClassDef_derived(self: ClassDef)(implicit ctx: Context): List[TypeTree] = ClassDef_rhs(self).derived.asInstanceOf[List[TypeTree]]
  def ClassDef_self(self: ClassDef)(implicit ctx: Context): Option[ValDef] = optional(ClassDef_rhs(self).self)
  def ClassDef_body(self: ClassDef)(implicit ctx: Context): List[Statement] = ClassDef_rhs(self).body
  def ClassDef_symbol(self: ClassDef)(implicit ctx: Context): ClassSymbol = self.symbol.asClass
  private def ClassDef_rhs(self: ClassDef) = self.rhs.asInstanceOf[tpd.Template]

  type TypeDef = tpd.TypeDef

  def TypeDef_rhs(self: TypeDef)(implicit ctx: Context): TypeOrBoundsTree = self.rhs
  def TypeDef_symbol(self: TypeDef)(implicit ctx: Context): TypeSymbol = self.symbol.asType

  type DefDef = tpd.DefDef

  def DefDef_typeParams(self: DefDef)(implicit ctx: Context): List[TypeDef] = self.tparams
  def DefDef_paramss(self: DefDef)(implicit ctx: Context): List[List[ValDef]] = self.vparamss
  def DefDef_returnTpt(self: DefDef)(implicit ctx: Context): TypeTree = self.tpt
  def DefDef_rhs(self: DefDef)(implicit ctx: Context): Option[Tree] = optional(self.rhs)
  def DefDef_symbol(self: DefDef)(implicit ctx: Context): DefSymbol = self.symbol.asTerm

  type ValDef = tpd.ValDef

  def ValDef_tpt(self: ValDef)(implicit ctx: Context): TypeTree = self.tpt
  def ValDef_rhs(self: ValDef)(implicit ctx: Context): Option[Tree] = optional(self.rhs)
  def ValDef_symbol(self: ValDef)(implicit ctx: Context): ValSymbol = self.symbol.asTerm

  type Term = tpd.Tree

  def Term_pos(self: Term)(implicit ctx: Context): Position = self.sourcePos
  def Term_tpe(self: Term)(implicit ctx: Context): Type = self.tpe
  def Term_underlyingArgument(self: Term)(implicit ctx: Context): Term = self.underlyingArgument
  def Term_underlying(self: Term)(implicit ctx: Context): Term = self.underlying

  type Term_Ref = tpd.RefTree

  type Term_Ident = tpd.Ident

  def Term_Ident_name(self: Term_Ident)(implicit ctx: Context): String = self.name.show

  type Term_Select = tpd.Select

  def Term_Select_qualifier(self: Term_Select)(implicit ctx: Context): Term = self.qualifier
  def Term_Select_name(self: Term_Select)(implicit ctx: Context): String = self.name.toString
  def Term_Select_signature(self: Term_Select)(implicit ctx: Context): Option[Signature] =
    if (self.symbol.signature == core.Signature.NotAMethod) None
    else Some(self.symbol.signature)

  type Term_Literal = tpd.Literal

  def Term_Literal_constant(self: Term_Literal)(implicit ctx: Context): Constant = self.const

  type Term_This = tpd.This

  def Term_This_id(self: Term_This)(implicit ctx: Context): Option[Id] = optional(self.qual)

  type Term_New = tpd.New

  def Term_New_tpt(self: Term_New)(implicit ctx: Context): TypeTree = self.tpt

  type Term_NamedArg = tpd.NamedArg

  def Term_NamedArg_name(self: Term_NamedArg)(implicit ctx: Context): String = self.name.toString
  def Term_NamedArg_value(self: Term_NamedArg)(implicit ctx: Context): Term = self.arg

  type Term_Apply = tpd.Apply

  def Term_Apply_fun(self: Term_Apply)(implicit ctx: Context): Term = self.fun
  def Term_Apply_args(self: Term_Apply)(implicit ctx: Context): List[Term] = self.args

  type Term_TypeApply = tpd.TypeApply

  def Term_TypeApply_fun(self: Term_TypeApply)(implicit ctx: Context): Term = self.fun
  def Term_TypeApply_args(self: Term_TypeApply)(implicit ctx: Context): List[TypeTree] = self.args

  type Term_Super = tpd.Super

  def Term_Super_qualifier(self: Term_Super)(implicit ctx: Context): Term = self.qual
  def Term_Super_id(self: Term_Super)(implicit ctx: Context): Option[Id] = optional(self.mix)

  type Term_Typed = tpd.Typed

  def Term_Typed_expr(self: Term_Typed)(implicit ctx: Context): Term = self.expr
  def Term_Typed_tpt(self: Term_Typed)(implicit ctx: Context): TypeTree = self.tpt

  type Term_Assign = tpd.Assign

  def Term_Assign_lhs(self: Term_Assign)(implicit ctx: Context): Term = self.lhs
  def Term_Assign_rhs(self: Term_Assign)(implicit ctx: Context): Term = self.rhs

  type Term_Block = tpd.Block

  def Term_Block_statements(self: Term_Block)(implicit ctx: Context): List[Statement] = self.stats
  def Term_Block_expr(self: Term_Block)(implicit ctx: Context): Term = self.expr

  type Term_Inlined = tpd.Inlined

  def Term_Inlined_call(self: Term_Inlined)(implicit ctx: Context): Option[TermOrTypeTree] = optional(self.call)
  def Term_Inlined_bindings(self: Term_Inlined)(implicit ctx: Context): List[Definition] = self.bindings
  def Term_Inlined_body(self: Term_Inlined)(implicit ctx: Context): Term = self.expansion

  type Term_Lambda = tpd.Closure

  def Term_Lambda_meth(self: Term_Lambda)(implicit ctx: Context): Term = self.meth
  def Term_Lambda_tptOpt(self: Term_Lambda)(implicit ctx: Context): Option[TypeTree] = optional(self.tpt)

  type Term_If = tpd.If

  def Term_If_cond(self: Term_If)(implicit ctx: Context): Term = self.cond
  def Term_If_thenp(self: Term_If)(implicit ctx: Context): Term = self.thenp
  def Term_If_elsep(self: Term_If)(implicit ctx: Context): Term = self.elsep

  type Term_Match = tpd.Match

  def Term_Match_scrutinee(self: Term_Match)(implicit ctx: Context): Term = self.selector
  def Term_Match_cases(self: Term_Match)(implicit ctx: Context): List[CaseDef] = self.cases

  type Term_Try = tpd.Try

  def Term_Try_body(self: Term_Try)(implicit ctx: Context): Term = self.expr
  def Term_Try_cases(self: Term_Try)(implicit ctx: Context): List[CaseDef] = self.cases
  def Term_Try_finalizer(self: Term_Try)(implicit ctx: Context): Option[Term] = optional(self.finalizer)

  type Term_Return = tpd.Return

  def Term_Return_expr(self: Term_Return)(implicit ctx: Context): Term = self.expr

  type Term_Repeated = tpd.SeqLiteral

  def Term_Repeated_elems(self: Term_Repeated)(implicit ctx: Context): List[Term] = self.elems
  def Term_Repeated_elemtpt(self: Term_Repeated)(implicit ctx: Context): TypeTree = self.elemtpt

  type Term_SelectOuter = tpd.Select

  def Term_SelectOuter_qualifier(self: Term_SelectOuter)(implicit ctx: Context): Term = self.qualifier
  def Term_SelectOuter_level(self: Term_SelectOuter)(implicit ctx: Context): Int = {
    val NameKinds.OuterSelectName(_, levels) = self.name
    levels
  }
  def Term_SelectOuter_tpe(self: Term_SelectOuter)(implicit ctx: Context): Type = self.tpe.stripTypeVar

  type Term_While = tpd.WhileDo

  def Term_While_cond(self: Term_While)(implicit ctx: Context): Term = self.cond
  def Term_While_body(self: Term_While)(implicit ctx: Context): Term = self.body

  //
  // CASES
  //

  type CaseDef = tpd.CaseDef

  def CaseDef_pattern(self: CaseDef)(implicit ctx: Context): Pattern = self.pat
  def CaseDef_guard(self: CaseDef)(implicit ctx: Context): Option[Term] = optional(self.guard)
  def CaseDef_rhs(self: CaseDef)(implicit ctx: Context): Term = self.body

  type TypeCaseDef = tpd.CaseDef

  def TypeCaseDef_pattern(self: TypeCaseDef)(implicit ctx: Context): TypeTree = self.pat
  def TypeCaseDef_rhs(self: TypeCaseDef)(implicit ctx: Context): TypeTree = self.body

  //
  // PATTERNS
  //

  type Pattern = tpd.Tree
  type Value = tpd.Tree

  def Pattern_Value_value(self: Value)(implicit ctx: Context): Term = self

  type Bind = tpd.Bind

  def Pattern_Bind_name(self: Bind)(implicit ctx: Context): String = self.name.toString

  def Pattern_Bind_pattern(self: Bind)(implicit ctx: Context): Pattern = self.body

  type Unapply = tpd.UnApply

  def Pattern_Unapply_fun(self: Unapply)(implicit ctx: Context): Term = self.fun
  def Pattern_Unapply_implicits(self: Unapply)(implicit ctx: Context): List[Term] = self.implicits
  def Pattern_Unapply_patterns(self: Unapply)(implicit ctx: Context): List[Pattern] = effectivePatterns(self.patterns)

  private def effectivePatterns(patterns: List[Pattern]): List[Pattern] = patterns match {
    case patterns0 :+ Trees.SeqLiteral(elems, _) => patterns0 ::: elems
    case _ => patterns
  }

  type Alternatives = tpd.Alternative

  def Pattern_Alternatives_patterns(self: Alternatives)(implicit ctx: Context): List[Pattern] = self.trees

  type TypeTest = tpd.Typed

  def Pattern_TypeTest_tpt(self: TypeTest)(implicit ctx: Context): TypeTree = self.tpt

  //
  // TYPE TREES
  //

  type TypeOrBoundsTree = tpd.Tree

  def TypeOrBoundsTree_tpe(self: TypeOrBoundsTree)(implicit ctx: Context): Type = self.tpe.stripTypeVar

  type TypeTree = tpd.Tree

  def TypeTree_pos(self: TypeTree)(implicit ctx: Context): Position = self.sourcePos
  def TypeTree_symbol(self: TypeTree)(implicit ctx: Context): Symbol = self.symbol
  def TypeTree_tpe(self: TypeTree)(implicit ctx: Context): Type = self.tpe.stripTypeVar

  type TypeTree_Inferred = tpd.TypeTree

  type TypeTree_Ident = tpd.Ident

  def TypeTree_Ident_name(self: TypeTree_Ident)(implicit ctx: Context): String = self.name.toString

  type TypeTree_Select = tpd.Select

  def TypeTree_Select_qualifier(self: TypeTree_Select)(implicit ctx: Context): Term = self.qualifier
  def TypeTree_Select_name(self: TypeTree_Select)(implicit ctx: Context): String = self.name.toString

  type TypeTree_Projection = tpd.Select

  def TypeTree_Projection_qualifier(self: TypeTree_Projection)(implicit ctx: Context): TypeTree = self.qualifier
  def TypeTree_Projection_name(self: TypeTree_Projection)(implicit ctx: Context): String = self.name.toString

  type TypeTree_Singleton = tpd.SingletonTypeTree

  def TypeTree_Singleton_ref(self: TypeTree_Singleton)(implicit ctx: Context): Term = self.ref

  type TypeTree_Refined = tpd.RefinedTypeTree

  def TypeTree_Refined_tpt(self: TypeTree_Refined)(implicit ctx: Context): TypeTree = self.tpt
  def TypeTree_Refined_refinements(self: TypeTree_Refined)(implicit ctx: Context): List[Definition] = self.refinements

  type TypeTree_Applied = tpd.AppliedTypeTree

  def TypeTree_Applied_tpt(self: TypeTree_Applied)(implicit ctx: Context): TypeTree = self.tpt
  def TypeTree_Applied_args(self: TypeTree_Applied)(implicit ctx: Context): List[TypeOrBoundsTree] = self.args

  type TypeTree_Annotated = tpd.Annotated

  def TypeTree_Annotated_arg(self: TypeTree_Annotated)(implicit ctx: Context): TypeTree = self.arg
  def TypeTree_Annotated_annotation(self: TypeTree_Annotated)(implicit ctx: Context): Term = self.annot

  type TypeTree_MatchType = tpd.MatchTypeTree

  def TypeTree_MatchType_bound(self: TypeTree_MatchType)(implicit ctx: Context): Option[TypeTree] = if (self.bound == tpd.EmptyTree) None else Some(self.bound)
  def TypeTree_MatchType_selector(self: TypeTree_MatchType)(implicit ctx: Context): TypeTree = self.selector
  def TypeTree_MatchType_cases(self: TypeTree_MatchType)(implicit ctx: Context): List[CaseDef] = self.cases

  type TypeTree_ByName = tpd.ByNameTypeTree

  def TypeTree_ByName_result(self: TypeTree_ByName)(implicit ctx: Context): TypeTree = self.result

  type TypeTree_LambdaTypeTree = tpd.LambdaTypeTree

  def TypeTree_LambdaTypeTree_tparams(self: TypeTree_LambdaTypeTree)(implicit ctx: Context): List[TypeDef] = self.tparams
  def TypeTree_LambdaTypeTree_body(self: TypeTree_LambdaTypeTree)(implicit ctx: Context): TypeOrBoundsTree = self.body

  type TypeTree_TypeBind = tpd.Bind

  def TypeTree_TypeBind_name(self: TypeTree_TypeBind)(implicit ctx: Context): String = self.name.toString
  def TypeTree_TypeBind_body(self: TypeTree_TypeBind)(implicit ctx: Context): TypeOrBoundsTree = self.body

  type TypeTree_TypeBlock = tpd.Block

  def TypeTree_TypeBlock_aliases(self: TypeTree_TypeBlock)(implicit ctx: Context): List[TypeDef] = self.stats.map { case alias: TypeDef => alias }
  def TypeTree_TypeBlock_tpt(self: TypeTree_TypeBlock)(implicit ctx: Context): TypeTree = self.expr

  type TypeBoundsTree = tpd.TypeBoundsTree

  def TypeBoundsTree_tpe(self: TypeBoundsTree)(implicit ctx: Context): TypeBounds = self.tpe.asInstanceOf[Types.TypeBounds]
  def TypeBoundsTree_low(self: TypeBoundsTree)(implicit ctx: Context): TypeTree = self.lo
  def TypeBoundsTree_hi(self: TypeBoundsTree)(implicit ctx: Context): TypeTree = self.hi

  type WildcardType = tpd.TypeTree

  //
  // TYPES
  //

  type TypeOrBounds = Types.Type

  type NoPrefix = Types.NoPrefix.type

  type TypeBounds = Types.TypeBounds

  def TypeBounds_low(self: TypeBounds)(implicit ctx: Context): Type = self.lo
  def TypeBounds_hi(self: TypeBounds)(implicit ctx: Context): Type = self.hi

  type Type = Types.Type

  def `Type_=:=`(self: Type)(that: Type)(implicit ctx: Context): Boolean = self =:= that

  def `Type_<:<`(self: Type)(that: Type)(implicit ctx: Context): Boolean = self <:< that

  /** Widen from singleton type to its underlying non-singleton
    *  base type by applying one or more `underlying` dereferences,
    *  Also go from => T to T.
    *  Identity for all other types. Example:
    *
    *  class Outer { class C ; val x: C }
    *  def o: Outer
    *  <o.x.type>.widen = o.C
    */
  def Type_widen(self: Type)(implicit ctx: Context): Type = self.widen

  def Type_classSymbol(self: Type)(implicit ctx: Context): Option[ClassSymbol] =
    if (self.classSymbol.exists) Some(self.classSymbol.asClass) else None

  def Type_typeSymbol(self: Type)(implicit ctx: Context): Symbol = self.typeSymbol

  def Type_isSingleton(self: Type)(implicit ctx: Context): Boolean = self.isSingleton

  def Type_memberType(self: Type)(member: Symbol)(implicit ctx: Context): Type =
    member.info.asSeenFrom(self, member.owner)

  type ConstantType = Types.ConstantType

  def ConstantType_value(self: ConstantType)(implicit ctx: Context): Any = self.value

  type SymRef = Types.NamedType

  def SymRef_qualifier(self: SymRef)(implicit ctx: Context): TypeOrBounds = self.prefix

  type TermRef = Types.NamedType

  def TermRef_qualifier(self: TermRef)(implicit ctx: Context): TypeOrBounds = self.prefix

  type TypeRef = Types.NamedType

  def TypeRef_name(self: TypeRef)(implicit ctx: Context): String = self.name.toString
  def TypeRef_qualifier(self: TypeRef)(implicit ctx: Context): TypeOrBounds = self.prefix

  type SuperType = Types.SuperType

  def SuperType_thistpe(self: SuperType)(implicit ctx: Context): Type = self.thistpe
  def SuperType_supertpe(self: SuperType)(implicit ctx: Context): Type = self.supertpe

  type Refinement = Types.RefinedType

  def Refinement_parent(self: Refinement)(implicit ctx: Context): Type = self.parent
  def Refinement_name(self: Refinement)(implicit ctx: Context): String = self.refinedName.toString
  def Refinement_info(self: Refinement)(implicit ctx: Context): TypeOrBounds = self.refinedInfo

  type AppliedType = Types.AppliedType

  def AppliedType_tycon(self: AppliedType)(implicit ctx: Context): Type = self.tycon
  def AppliedType_args(self: AppliedType)(implicit ctx: Context): List[TypeOrBounds] = self.args

  type AnnotatedType = Types.AnnotatedType

  def AnnotatedType_underlying(self: AnnotatedType)(implicit ctx: Context): Type = self.underlying.stripTypeVar
  def AnnotatedType_annot(self: AnnotatedType)(implicit ctx: Context): Term = self.annot.tree

  type AndType = Types.AndType

  def AndType_left(self: AndType)(implicit ctx: Context): Type = self.tp1.stripTypeVar
  def AndType_right(self: AndType)(implicit ctx: Context): Type = self.tp2.stripTypeVar

  type OrType = Types.OrType

  def OrType_left(self: OrType)(implicit ctx: Context): Type = self.tp1.stripTypeVar
  def OrType_right(self: OrType)(implicit ctx: Context): Type = self.tp2.stripTypeVar

  type MatchType = Types.MatchType

  def MatchType_bound(self: MatchType)(implicit ctx: Context): Type = self.bound
  def MatchType_scrutinee(self: MatchType)(implicit ctx: Context): Type = self.scrutinee
  def MatchType_cases(self: MatchType)(implicit ctx: Context): List[Type] = self.cases

  type ByNameType = Types.ExprType

  def ByNameType_underlying(self: ByNameType)(implicit ctx: Context): Type = self.resType.stripTypeVar

  type ParamRef = Types.ParamRef

  def ParamRef_binder(self: ParamRef)(implicit ctx: Context): LambdaType[TypeOrBounds] =
    self.binder.asInstanceOf[LambdaType[TypeOrBounds]] // Cast to tpd
  def ParamRef_paramNum(self: ParamRef)(implicit ctx: Context): Int = self.paramNum

  type ThisType = Types.ThisType

  def ThisType_underlying(self: ThisType)(implicit ctx: Context): Type = self.underlying

  type RecursiveThis = Types.RecThis

  def RecursiveThis_binder(self: RecursiveThis)(implicit ctx: Context): RecursiveType = self.binder

  type RecursiveType = Types.RecType

  def RecursiveType_underlying(self: RecursiveType)(implicit ctx: Context): Type = self.underlying.stripTypeVar

  type LambdaType[ParamInfo] = Types.LambdaType { type PInfo = ParamInfo }

  type MethodType = Types.MethodType

  def MethodType_isErased(self: MethodType): Boolean = self.isErasedMethod
  def MethodType_isImplicit(self: MethodType): Boolean = self.isImplicitMethod
  def MethodType_paramNames(self: MethodType)(implicit ctx: Context): List[String] = self.paramNames.map(_.toString)
  def MethodType_paramTypes(self: MethodType)(implicit ctx: Context): List[Type] = self.paramInfos
  def MethodType_resType(self: MethodType)(implicit ctx: Context): Type = self.resType

  type PolyType = Types.PolyType

  def PolyType_paramNames(self: PolyType)(implicit ctx: Context): List[String] = self.paramNames.map(_.toString)
  def PolyType_paramBounds(self: PolyType)(implicit ctx: Context): List[TypeBounds] = self.paramInfos
  def PolyType_resType(self: PolyType)(implicit ctx: Context): Type = self.resType

  type TypeLambda = Types.TypeLambda

  def TypeLambda_paramNames(self: TypeLambda)(implicit ctx: Context): List[String] = self.paramNames.map(_.toString)
  def TypeLambda_paramBounds(self: TypeLambda)(implicit ctx: Context): List[TypeBounds] = self.paramInfos
  def TypeLambda_resType(self: TypeLambda)(implicit ctx: Context): Type = self.resType

  //
  // IMPORT SELECTORS
  //

  type ImportSelector = untpd.Tree

  //
  // IDENTIFIERS
  //

  type Id = untpd.Ident

  def Id_pos(self: Id)(implicit ctx: Context): Position = self.sourcePos

  def Id_name(self: Id)(implicit ctx: Context): String = self.name.toString

  //
  // SIGNATURES
  //

  type Signature = core.Signature

  def Signature_paramSigs(self: Signature): List[String] =
    self.paramsSig.map(_.toString)

  def Signature_resultSig(self: Signature): String =
    self.resSig.toString

  //
  // POSITIONS
  //

  type Position = util.SourcePosition

  def Position_start(self: Position): Int = self.start

  def Position_end(self: Position): Int = self.end

  def Position_exists(self: Position): Boolean = self.exists

  def Position_sourceFile(self: Position): java.nio.file.Path = self.source.file.jpath

  def Position_startLine(self: Position): Int = self.startLine

  def Position_endLine(self: Position): Int = self.endLine

  def Position_startColumn(self: Position): Int = self.startColumn

  def Position_endColumn(self: Position): Int = self.endColumn

  def Position_sourceCode(self: Position): String =
    new String(self.source.content(), self.start, self.end - self.start)

  //
  // COMMENTS
  //

  type Comment = core.Comments.Comment

  //
  // CONSTANTS
  //

  type Constant = Constants.Constant

  final def Constant_value(const: Constant): Any = const.value

  //
  // SYMBOLS
  //

  type Symbol = core.Symbols.Symbol

  def Symbol_owner(self: Symbol)(implicit ctx: Context): Symbol = self.owner

  def Symbol_flags(self: Symbol)(implicit ctx: Context): Flags = self.flags


  def Symbol_privateWithin(self: Symbol)(implicit ctx: Context): Option[Type] = {
    val within = self.privateWithin
    if (within.exists && !self.is(core.Flags.Protected)) Some(within.typeRef)
    else None
  }

  def Symbol_protectedWithin(self: Symbol)(implicit ctx: Context): Option[Type] = {
    val within = self.privateWithin
    if (within.exists && self.is(core.Flags.Protected)) Some(within.typeRef)
    else None
  }

  def Symbol_name(self: Symbol)(implicit ctx: Context): String = self.name.toString

  def Symbol_fullName(self: Symbol)(implicit ctx: Context): String = self.fullName.toString

  def Symbol_pos(self: Symbol)(implicit ctx: Context): Position = self.sourcePos

  def Symbol_localContext(self: Symbol)(implicit ctx: Context): Context = {
    if (self.exists) ctx.withOwner(self)
    else ctx
  }

  def Symbol_comment(self: Symbol)(implicit ctx: Context): Option[Comment] = {
    import dotty.tools.dotc.core.Comments.CommentsContext
    val docCtx = ctx.docCtx.getOrElse {
      throw new RuntimeException(
        "DocCtx could not be found and comments are unavailable. This is a compiler-internal error."
      )
    }
    docCtx.docstring(self)
  }
  def Symbol_annots(self: Symbol)(implicit ctx: Context): List[Term] = {
    self.annotations.flatMap {
      case _: core.Annotations.LazyBodyAnnotation => Nil
      case annot => annot.tree :: Nil
    }
  }

  def Symbol_isDefinedInCurrentRun(self: Symbol)(implicit ctx: Context): Boolean =
    self.topLevelClass.asClass.isDefinedInCurrentRun

  def Symbol_isLocalDummy(self: Symbol)(implicit ctx: Context): Boolean = self.isLocalDummy
  def Symbol_isRefinementClass(self: Symbol)(implicit ctx: Context): Boolean = self.isRefinementClass
  def Symbol_isAliasType(self: Symbol)(implicit ctx: Context): Boolean = self.isAliasType
  def Symbol_isAnonymousClass(self: Symbol)(implicit ctx: Context): Boolean = self.isAnonymousClass
  def Symbol_isAnonymousFunction(self: Symbol)(implicit ctx: Context): Boolean = self.isAnonymousFunction
  def Symbol_isAbstractType(self: Symbol)(implicit ctx: Context): Boolean = self.isAbstractType
  def Symbol_isClassConstructor(self: Symbol)(implicit ctx: Context): Boolean = self.isClassConstructor

  type PackageSymbol = core.Symbols.Symbol
  type ClassSymbol = core.Symbols.ClassSymbol
  type TypeSymbol = core.Symbols.TypeSymbol
  type DefSymbol = core.Symbols.TermSymbol
  type BindSymbol = core.Symbols.TermSymbol
  type ValSymbol = core.Symbols.TermSymbol
  type NoSymbol = core.Symbols.NoSymbol.type

  //
  // FLAGS
  //

  type Flags = core.Flags.FlagSet

  /** Is the given flag set a subset of this flag sets */
  def Flags_is(self: Flags)(that: Flags): Boolean = self.is(that)

  /** Union of the two flag sets */
  def Flags_or(self: Flags)(that: Flags): Flags = self | that

  /** Intersection of the two flag sets */
  def Flags_and(self: Flags)(that: Flags): Flags = self & that

  def Flags_Private: Flags = core.Flags.Private
  def Flags_Protected: Flags = core.Flags.Protected
  def Flags_Abstract: Flags = core.Flags.Abstract
  def Flags_Final: Flags = core.Flags.Final
  def Flags_Sealed: Flags = core.Flags.Sealed
  def Flags_Case: Flags = core.Flags.Case
  def Flags_Implicit: Flags = core.Flags.Implicit
  def Flags_Implied: Flags = core.Flags.Implied
  def Flags_Erased: Flags = core.Flags.Erased
  def Flags_Lazy: Flags = core.Flags.Lazy
  def Flags_Override: Flags = core.Flags.Override
  def Flags_Inline: Flags = core.Flags.Inline
  def Flags_Macro: Flags = core.Flags.Macro
  def Flags_Static: Flags = core.Flags.JavaStatic
  def Flags_JavaDefined: Flags = core.Flags.JavaDefined
  def Flags_Object: Flags = core.Flags.Module
  def Flags_Trait: Flags = core.Flags.Trait
  def Flags_Local: Flags = core.Flags.Local
  def Flags_Synthetic: Flags = core.Flags.Synthetic
  def Flags_Artifact: Flags = core.Flags.Artifact
  def Flags_Mutable: Flags = core.Flags.Mutable
  def Flags_FieldAccessor: Flags = core.Flags.Accessor
  def Flags_CaseAcessor: Flags = core.Flags.CaseAccessor
  def Flags_Covariant: Flags = core.Flags.Covariant
  def Flags_Contravariant: Flags = core.Flags.Contravariant
  def Flags_Scala2X: Flags = core.Flags.Scala2x
  def Flags_DefaultParameterized: Flags = core.Flags.DefaultParameterized
  def Flags_StableRealizable: Flags = core.Flags.StableRealizable
  def Flags_Param: Flags = core.Flags.Param
  def Flags_ParamAccessor: Flags = core.Flags.ParamAccessor
  def Flags_Enum: Flags = core.Flags.Enum
  def Flags_ModuleClass: Flags = core.Flags.ModuleClass
  def Flags_PrivateLocal: Flags = core.Flags.PrivateLocal
  def Flags_Package: Flags = core.Flags.Package
  def Flags_ImplClass: Flags = core.Flags.ImplClass

  //
  // QUOTED SEAL/UNSEAL
  //

  /** View this expression `Expr[_]` as a `Term` */
  def QuotedExpr_unseal(self: scala.quoted.Expr[_])(implicit ctx: Context): Term =
    PickledQuotes.quotedExprToTree(self)

  /** View this expression `Type[T]` as a `TypeTree` */
  def QuotedType_unseal(self: scala.quoted.Type[_])(implicit ctx: Context): TypeTree =
    PickledQuotes.quotedTypeToTree(self)

  /** Convert `Term` to an `Expr[T]` and check that it conforms to `T` */
  def QuotedExpr_seal[T](self: Term)(tpe: scala.quoted.Type[T])(implicit ctx: Context): scala.quoted.Expr[T] = {

    val expectedType = QuotedType_unseal(tpe).tpe

    def etaExpand(term: Term): Term = term.tpe.widen match {
      case mtpe: Types.MethodType if !mtpe.isParamDependent =>
        val closureResType = mtpe.resType match {
          case t: Types.MethodType => t.toFunctionType()
          case t => t
        }
        val closureTpe = Types.MethodType(mtpe.paramNames, mtpe.paramInfos, closureResType)
        val closureMethod = ctx.newSymbol(ctx.owner, nme.ANON_FUN, Synthetic | Method, closureTpe)
        tpd.Closure(closureMethod, tss => etaExpand(new tpd.TreeOps(term).appliedToArgs(tss.head)))
      case _ => term
    }

    val expanded = etaExpand(self)
    if (expanded.tpe <:< expectedType) {
      new scala.quoted.Exprs.TastyTreeExpr(expanded).asInstanceOf[scala.quoted.Expr[T]]
    } else {
      throw new scala.tasty.TastyTypecheckError(
        s"""Term: ${self.show}
           |did not conform to type: ${expectedType.show}
           |""".stripMargin
      )
    }
  }

  /** Convert `Type` to an `quoted.Type[T]` */
  def QuotedType_seal(self: Type)(implicit ctx: Context): scala.quoted.Type[_] = {
    val dummySpan = ctx.owner.span // FIXME
    new scala.quoted.Types.TreeType(tpd.TypeTree(self).withSpan(dummySpan))
  }

  //
  // HELPERS
  //

  private def optional[T <: Trees.Tree[_]](tree: T): Option[tree.type] =
    if (tree.isEmpty) None else Some(tree)

}
