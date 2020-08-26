package scala.internal.tasty

import scala.quoted.QuoteContext
import scala.tasty.reflect._
import scala.internal.quoted.Unpickler

/** Part of the reflection interface that needs to be implemented by the compiler */
trait CompilerInterface extends scala.tasty.reflect.Types {

  /** Context of the macro expansion */
  def rootContext: Context

  /** Root position of this tasty context. For macros it corresponds to the expansion site. */
  def rootPosition: Position


  //////////////////////
  // QUOTE UNPICKLING //
  //////////////////////

  /** Unpickle `repr` which represents a pickled `Expr` tree,
   *  replacing splice nodes with `args`
   */
  def unpickleExpr(repr: Unpickler.PickledQuote, args: Unpickler.PickledArgs): Term

  /** Unpickle `repr` which represents a pickled `Type` tree,
   *  replacing splice nodes with `args`
   */
  def unpickleType(repr: Unpickler.PickledQuote, args: Unpickler.PickledArgs): TypeTree


  /////////////////
  // Constraints //
  /////////////////

  def Constraints_context[T]: scala.quoted.QuoteContext
  def Constraints_add(self: Context)(syms: List[Symbol]): Boolean
  def Constraints_approximation(self: Context)(sym: Symbol, fromBelow: Boolean): Type

  ////////////
  // Source //
  ////////////

  /** Returns the source file being compiled. The path is relative to the current working directory. */
  def Source_path: java.nio.file.Path

  /** Returns true if we've tried to reflect on a Java class. */
  def Source_isJavaCompilationUnit: Boolean

  /** Returns true if we've tried to reflect on a Scala2 (non-Tasty) class. */
  def Source_isScala2CompilationUnit: Boolean

  /** Returns true if we've tried to reflect on a class that's already loaded (e.g. Option). */
  def Source_isAlreadyLoadedCompilationUnit: Boolean

  /** Class name of the current CompilationUnit */
  def Source_compilationUnitClassname: String

  ///////////////
  // REPORTING //
  ///////////////

  /** Report a compilation error with the given message at the given position */
  def error(msg: => String, pos: Position): Unit

  /** Report a compilation error with the given message at the given position range */
  def error(msg: => String, source: SourceFile, start: Int, end: Int): Unit

  /** Report a compilation warning with the given message at the given position */
  def warning(msg: => String, pos: Position): Unit

  /** Report a compilation warning with the given message at the given position range */
  def warning(msg: => String, source: SourceFile, start: Int, end: Int): Unit


  /////////////
  //  TREES  //
  /////////////

  def Tree_pos(self: Tree): Position
  def Tree_symbol(self: Tree): Symbol

  def PackageClause_TypeTest: TypeTest[Tree, PackageClause]

  def PackageClause_pid(self: PackageClause): Ref
  def PackageClause_stats(self: PackageClause): List[Tree]

  def PackageClause_apply(pid: Ref, stats: List[Tree]): PackageClause

  def PackageClause_copy(original: Tree)(pid: Ref, stats: List[Tree]): PackageClause

  def Statement_TypeTest: TypeTest[Tree, Statement]

  def Import_TypeTest: TypeTest[Tree, Import]

  def Import_implied(self: Import): Boolean
  def Import_expr(self: Import): Term
  def Import_selectors(self: Import): List[ImportSelector]

  def Import_apply(iexpr: Term, selectors: List[ImportSelector]): Import

  def Import_copy(original: Tree)(expr: Term, selectors: List[ImportSelector]): Import

  def Definition_TypeTest: TypeTest[Tree, Definition]

  def Definition_name(self: Definition): String

  def PackageDef_TypeTest: TypeTest[Tree, PackageDef]

  def PackageDef_owner(self: PackageDef): PackageDef
  def PackageDef_members(self: PackageDef): List[Statement]

  def ClassDef_TypeTest: TypeTest[Tree, ClassDef]

  def ClassDef_constructor(self: ClassDef): DefDef
  def ClassDef_parents(self: ClassDef): List[Tree/* Term | TypeTree */]
  def ClassDef_derived(self: ClassDef): List[TypeTree]
  def ClassDef_self(self: ClassDef): Option[ValDef]
  def ClassDef_body(self: ClassDef): List[Statement]

  def ClassDef_copy(original: Tree)(name: String, constr: DefDef, parents: List[Tree/* Term | TypeTree */], derived: List[TypeTree], selfOpt: Option[ValDef], body: List[Statement]): ClassDef

  def TypeDef_TypeTest: TypeTest[Tree, TypeDef]

  def TypeDef_rhs(self: TypeDef): Tree /*TypeTree | TypeBoundsTree*/

  def TypeDef_apply(symbol: Symbol): TypeDef
  def TypeDef_copy(original: Tree)(name: String, rhs: Tree /*TypeTree | TypeBoundsTree*/): TypeDef

  def DefDef_TypeTest: TypeTest[Tree, DefDef]

  def DefDef_typeParams(self: DefDef): List[TypeDef]
  def DefDef_paramss(self: DefDef): List[List[ValDef]]
  def DefDef_returnTpt(self: DefDef): TypeTree
  def DefDef_rhs(self: DefDef): Option[Term]

  def DefDef_apply(symbol: Symbol, rhsFn: List[Type] => List[List[Term]] => Option[Term]): DefDef
  def DefDef_copy(original: Tree)(name: String, typeParams: List[TypeDef], paramss: List[List[ValDef]], tpt: TypeTree, rhs: Option[Term]): DefDef

  def ValDef_TypeTest: TypeTest[Tree, ValDef]

  def ValDef_tpt(self: ValDef): TypeTree
  def ValDef_rhs(self: ValDef): Option[Term]

  def ValDef_apply(symbol: Symbol, rhs: Option[Term]): ValDef
  def ValDef_copy(original: Tree)(name: String, tpt: TypeTree, rhs: Option[Term]): ValDef

  def Term_TypeTest: TypeTest[Tree, Term]

  def Term_tpe(self: Term): Type
  def Term_underlyingArgument(self: Term): Term
  def Term_underlying(self: Term): Term
  def Term_etaExpand(term: Term): Term

  def Ref_TypeTest: TypeTest[Tree, Ref]

  /** A tree representing the same reference as the given type */
  def Ref_term(tp: TermRef): Ref

  def Ref_apply(sym: Symbol): Ref

  def Ident_TypeTest: TypeTest[Tree, Ident]

  def Ident_name(self: Ident): String

  def Ident_apply(tmref: TermRef): Term
  def Ident_copy(original: Tree)(name: String): Ident

  def Select_TypeTest: TypeTest[Tree, Select]

  def Select_qualifier(self: Select): Term
  def Select_name(self: Select): String
  def Select_signature(self: Select): Option[Signature]

  def Select_apply(qualifier: Term, symbol: Symbol): Select
  def Select_unique(qualifier: Term, name: String): Select
  // TODO rename, this returns an Apply and not a Select
  def Select_overloaded(qualifier: Term, name: String, targs: List[Type], args: List[Term]): Apply
  def Select_copy(original: Tree)(qualifier: Term, name: String): Select

  def Literal_TypeTest: TypeTest[Tree, Literal]

  def Literal_constant(self: Literal): Constant

  def Literal_apply(constant: Constant): Literal
  def Literal_copy(original: Tree)(constant: Constant): Literal

  def This_TypeTest: TypeTest[Tree, This]

  def This_id(self: This): Option[Id]

  def This_apply(cls: Symbol): This
  def This_copy(original: Tree)(qual: Option[Id]): This

  def New_TypeTest: TypeTest[Tree, New]

  def New_tpt(self: New): TypeTree

  def New_apply(tpt: TypeTree): New
  def New_copy(original: Tree)(tpt: TypeTree): New

  def NamedArg_TypeTest: TypeTest[Tree, NamedArg]

  def NamedArg_name(self: NamedArg): String
  def NamedArg_value(self: NamedArg): Term

  def NamedArg_apply(name: String, arg: Term): NamedArg
  def NamedArg_copy(original: Tree)(name: String, arg: Term): NamedArg

  def Apply_TypeTest: TypeTest[Tree, Apply]

  def Apply_fun(self: Apply): Term
  def Apply_args(self: Apply): List[Term]

  def Apply_apply(fn: Term, args: List[Term]): Apply
  def Apply_copy(original: Tree)(fun: Term, args: List[Term]): Apply

  def TypeApply_TypeTest: TypeTest[Tree, TypeApply]

  def TypeApply_fun(self: TypeApply): Term
  def TypeApply_args(self: TypeApply): List[TypeTree]

  def TypeApply_apply(fn: Term, args: List[TypeTree]): TypeApply
  def TypeApply_copy(original: Tree)(fun: Term, args: List[TypeTree]): TypeApply

  def Super_TypeTest: TypeTest[Tree, Super]

  def Super_qualifier(self: Super): Term
  def Super_id(self: Super): Option[Id]

  def Super_apply(qual: Term, mix: Option[Id]): Super
  def Super_copy(original: Tree)(qual: Term, mix: Option[Id]): Super

  def Typed_TypeTest: TypeTest[Tree, Typed]

  def Typed_expr(self: Typed): Term
  def Typed_tpt(self: Typed): TypeTree

  def Typed_apply(expr: Term, tpt: TypeTree): Typed
  def Typed_copy(original: Tree)(expr: Term, tpt: TypeTree): Typed

  def Assign_TypeTest: TypeTest[Tree, Assign]

  def Assign_lhs(self: Assign): Term
  def Assign_rhs(self: Assign): Term

  def Assign_apply(lhs: Term, rhs: Term): Assign
  def Assign_copy(original: Tree)(lhs: Term, rhs: Term): Assign

  def Block_TypeTest: TypeTest[Tree, Block]

  def Block_statements(self: Block): List[Statement]
  def Block_expr(self: Block): Term

  def Block_apply(stats: List[Statement], expr: Term): Block
  def Block_copy(original: Tree)(stats: List[Statement], expr: Term): Block

  def Closure_TypeTest: TypeTest[Tree, Closure]

  def Closure_meth(self: Closure): Term
  def Closure_tpeOpt(self: Closure): Option[Type]

  def Closure_apply(meth: Term, tpe: Option[Type]): Closure
  def Closure_copy(original: Tree)(meth: Tree, tpe: Option[Type]): Closure

  def Lambda_apply(tpe: MethodType, rhsFn: List[Tree] => Tree): Block

  def If_TypeTest: TypeTest[Tree, If]

  def If_cond(self: If): Term
  def If_thenp(self: If): Term
  def If_elsep(self: If): Term

  def If_apply(cond: Term, thenp: Term, elsep: Term): If
  def If_copy(original: Tree)(cond: Term, thenp: Term, elsep: Term): If

  def Match_TypeTest: TypeTest[Tree, Match]

  def Match_scrutinee(self: Match): Term
  def Match_cases(self: Match): List[CaseDef]

  def Match_apply(selector: Term, cases: List[CaseDef]): Match
  def Match_copy(original: Tree)(selector: Term, cases: List[CaseDef]): Match

  def GivenMatch_TypeTest: TypeTest[Tree, GivenMatch]

  def GivenMatch_cases(self: GivenMatch): List[CaseDef]

  def GivenMatch_apply(cases: List[CaseDef]): GivenMatch
  def GivenMatch_copy(original: Tree)(cases: List[CaseDef]): GivenMatch

  def Try_TypeTest: TypeTest[Tree, Try]

  def Try_body(self: Try): Term
  def Try_cases(self: Try): List[CaseDef]
  def Try_finalizer(self: Try): Option[Term]

  def Try_apply(expr: Term, cases: List[CaseDef], finalizer: Option[Term]): Try
  def Try_copy(original: Tree)(expr: Term, cases: List[CaseDef], finalizer: Option[Term]): Try

  def Return_TypeTest: TypeTest[Tree, Return]

  def Return_expr(self: Return): Term

  def Return_apply(expr: Term): Return
  def Return_copy(original: Tree)(expr: Term): Return

  def Repeated_TypeTest: TypeTest[Tree, Repeated]

  def Repeated_elems(self: Repeated): List[Term]
  def Repeated_elemtpt(self: Repeated): TypeTree

  def Repeated_apply(elems: List[Term], elemtpt: TypeTree): Repeated
  def Repeated_copy(original: Tree)(elems: List[Term], elemtpt: TypeTree): Repeated

  def Inlined_TypeTest: TypeTest[Tree, Inlined]

  def Inlined_call(self: Inlined): Option[Tree/* Term | TypeTree */]
  def Inlined_bindings(self: Inlined): List[Definition]
  def Inlined_body(self: Inlined): Term

  def Inlined_apply(call: Option[Tree/* Term | TypeTree */], bindings: List[Definition], expansion: Term): Inlined
  def Inlined_copy(original: Tree)(call: Option[Tree/* Term | TypeTree */], bindings: List[Definition], expansion: Term): Inlined

  def SelectOuter_TypeTest: TypeTest[Tree, SelectOuter]

  def SelectOuter_qualifier(self: SelectOuter): Term
  def SelectOuter_level(self: SelectOuter): Int

  def SelectOuter_apply(qualifier: Term, name: String, levels: Int): SelectOuter
  def SelectOuter_copy(original: Tree)(qualifier: Term, name: String, levels: Int): SelectOuter

  def While_TypeTest: TypeTest[Tree, While]

  def While_cond(self: While): Term
  def While_body(self: While): Term

  def While_apply(cond: Term, body: Term): While
  def While_copy(original: Tree)(cond: Term, body: Term): While

  def TypeTree_TypeTest: TypeTest[Tree, TypeTree]

  def TypeTree_tpe(self: TypeTree): Type

  def Inferred_TypeTest: TypeTest[Tree, Inferred]

  def Inferred_apply(tpe: Type): Inferred

  def TypeRef_apply(sym: Symbol): TypeTree

  def TypeIdent_TypeTest: TypeTest[Tree, TypeIdent]

  def TypeIdent_name(self: TypeIdent): String

  def TypeIdent_copy(original: Tree)(name: String): TypeIdent

  def TypeSelect_TypeTest: TypeTest[Tree, TypeSelect]

  def TypeSelect_qualifier(self: TypeSelect): Term
  def TypeSelect_name(self: TypeSelect): String

  def TypeSelect_apply(qualifier: Term, name: String): TypeSelect
  def TypeSelect_copy(original: Tree)(qualifier: Term, name: String): TypeSelect

  def Projection_TypeTest: TypeTest[Tree, Projection]

  def Projection_qualifier(self: Projection): TypeTree
  def Projection_name(self: Projection): String

  def Projection_copy(original: Tree)(qualifier: TypeTree, name: String): Projection

  def Singleton_TypeTest: TypeTest[Tree, Singleton]

  def Singleton_ref(self: Singleton): Term

  def Singleton_apply(ref: Term): Singleton
  def Singleton_copy(original: Tree)(ref: Term): Singleton

  def Refined_TypeTest: TypeTest[Tree, Refined]

  def Refined_tpt(self: Refined): TypeTree
  def Refined_refinements(self: Refined): List[Definition]

  def Refined_copy(original: Tree)(tpt: TypeTree, refinements: List[Definition]): Refined

  def Applied_TypeTest: TypeTest[Tree, Applied]

  def Applied_tpt(self: Applied): TypeTree
  def Applied_args(self: Applied): List[Tree /*TypeTree | TypeBoundsTree*/]

  def Applied_apply(tpt: TypeTree, args: List[Tree /*TypeTree | TypeBoundsTree*/]): Applied
  def Applied_copy(original: Tree)(tpt: TypeTree, args: List[Tree /*TypeTree | TypeBoundsTree*/]): Applied

  def Annotated_TypeTest: TypeTest[Tree, Annotated]

  def Annotated_arg(self: Annotated): TypeTree
  def Annotated_annotation(self: Annotated): Term

  def Annotated_apply(arg: TypeTree, annotation: Term): Annotated
  def Annotated_copy(original: Tree)(arg: TypeTree, annotation: Term): Annotated

  def MatchTypeTree_TypeTest: TypeTest[Tree, MatchTypeTree]

  def MatchTypeTree_bound(self: MatchTypeTree): Option[TypeTree]
  def MatchTypeTree_selector(self: MatchTypeTree): TypeTree
  def MatchTypeTree_cases(self: MatchTypeTree): List[TypeCaseDef]

  def MatchTypeTree_apply(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef]): MatchTypeTree
  def MatchTypeTree_copy(original: Tree)(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef]): MatchTypeTree

  def ByName_result(self: ByName): TypeTree

  def ByName_TypeTest: TypeTest[Tree, ByName]

  def ByName_apply(result: TypeTree): ByName
  def ByName_copy(original: Tree)(result: TypeTree): ByName

  def LambdaTypeTree_TypeTest: TypeTest[Tree, LambdaTypeTree]

  def Lambdatparams(self: LambdaTypeTree): List[TypeDef]
  def Lambdabody(self: LambdaTypeTree): Tree /*TypeTree | TypeBoundsTree*/

  def Lambdaapply(tparams: List[TypeDef], body: Tree /*TypeTree | TypeBoundsTree*/): LambdaTypeTree
  def Lambdacopy(original: Tree)(tparams: List[TypeDef], body: Tree /*TypeTree | TypeBoundsTree*/): LambdaTypeTree

  def TypeBind_TypeTest: TypeTest[Tree, TypeBind]

  def TypeBind_name(self: TypeBind): String
  def TypeBind_body(self: TypeBind): Tree /*TypeTree | TypeBoundsTree*/

  def TypeBind_copy(original: Tree)(name: String, tpt: Tree /*TypeTree | TypeBoundsTree*/): TypeBind

  def TypeBlock_TypeTest: TypeTest[Tree, TypeBlock]

  def TypeBlock_aliases(self: TypeBlock): List[TypeDef]
  def TypeBlock_tpt(self: TypeBlock): TypeTree

  def TypeBlock_apply(aliases: List[TypeDef], tpt: TypeTree): TypeBlock
  def TypeBlock_copy(original: Tree)(aliases: List[TypeDef], tpt: TypeTree): TypeBlock

  def TypeBoundsTree_TypeTest: TypeTest[Tree, TypeBoundsTree]

  def TypeBoundsTree_tpe(self: TypeBoundsTree): TypeBounds
  def TypeBoundsTree_low(self: TypeBoundsTree): TypeTree
  def TypeBoundsTree_hi(self: TypeBoundsTree): TypeTree

  def WildcardTypeTree_TypeTest: TypeTest[Tree, WildcardTypeTree]

  def WildcardTypeTree_tpe(self: WildcardTypeTree): TypeOrBounds

  def CaseDef_TypeTest: TypeTest[Tree, CaseDef]

  def CaseDef_pattern(self: CaseDef): Tree
  def CaseDef_guard(self: CaseDef): Option[Term]
  def CaseDef_rhs(self: CaseDef): Term

  def CaseDef_module_apply(pattern: Tree, guard: Option[Term], body: Term): CaseDef
  def CaseDef_module_copy(original: Tree)(pattern: Tree, guard: Option[Term], body: Term): CaseDef

  def TypeCaseDef_TypeTest: TypeTest[Tree, TypeCaseDef]

  def TypeCaseDef_pattern(self: TypeCaseDef): TypeTree
  def TypeCaseDef_rhs(self: TypeCaseDef): TypeTree

  def TypeCaseDef_module_apply(pattern: TypeTree, body: TypeTree): TypeCaseDef
  def TypeCaseDef_module_copy(original: Tree)(pattern: TypeTree, body: TypeTree): TypeCaseDef

  //
  // PATTERNS
  //

  def Bind_TypeTest: TypeTest[Tree, Bind]

  def Tree_Bind_name(self: Bind): String

  def Tree_Bind_pattern(self: Bind): Tree

  def Tree_Bind_module_apply(sym: Symbol, body: Tree): Bind

  def Tree_Bind_module_copy(original: Tree)(name: String, pattern: Tree): Bind

  def Unapply_TypeTest: TypeTest[Tree, Unapply]

  def Tree_Unapply_fun(self: Unapply): Term

  def Tree_Unapply_implicits(self: Unapply): List[Term]

  def Tree_Unapply_patterns(self: Unapply): List[Tree]

  def Tree_Unapply_module_copy(original: Tree)(fun: Term, implicits: List[Term], patterns: List[Tree]): Unapply

  def Alternatives_TypeTest: TypeTest[Tree, Alternatives]

  def Tree_Alternatives_patterns(self: Alternatives): List[Tree]

  def Tree_Alternatives_module_apply(patterns: List[Tree]): Alternatives
  def Tree_Alternatives_module_copy(original: Tree)(patterns: List[Tree]): Alternatives


  //
  // TYPES
  //

  def NoPrefix_TypeTest: TypeTest[Tree, NoPrefix]

  def TypeBounds_TypeTest: TypeTest[TypeOrBounds, TypeBounds]

  def TypeBounds_apply(low: Type, hi: Type)(using ctx: Context): TypeBounds

  def TypeBounds_low(self: TypeBounds)(using ctx: Context): Type
  def TypeBounds_hi(self: TypeBounds)(using ctx: Context): Type

  def Type_TypeTest: TypeTest[TypeOrBounds, Type]

  def Type_apply(clazz: Class[_])(using ctx: Context): Type

  /** Is `self` type the same as `that` type?
   *  This is the case iff `Type_isSubType(self, that)` and `Type_isSubType(that, self)`.
   */
  def Type_isTypeEq(self: Type)(that: Type)(using ctx: Context): Boolean

  /** Is this type a subtype of that type? */
  def Type_isSubType(self: Type)(that: Type)(using ctx: Context): Boolean

  /** Widen from singleton type to its underlying non-singleton
    *  base type by applying one or more `underlying` dereferences,
    *  Also go from => T to T.
    *  Identity for all other types. Example:
    *
    *  class Outer { class C ; val x: C }
    *  def o: Outer
    *  <o.x.type>.widen = o.C
    */
  def Type_widen(self: Type)(using ctx: Context): Type

  /** Widen from TermRef to its underlying non-termref
   *  base type, while also skipping Expr types.
   */
  def Type_widenTermRefExpr(self: Type)(using ctx: Context): Type

  /** Follow aliases and dereferences LazyRefs, annotated types and instantiated
   *  TypeVars until type is no longer alias type, annotated type, LazyRef,
   *  or instantiated type variable.
   */
  def Type_dealias(self: Type)(using ctx: Context): Type

  def Type_simplified(self: Type)(using ctx: Context): Type

  def Type_classSymbol(self: Type)(using ctx: Context): Option[Symbol] // TODO remove Option and use NoSymbol

  def Type_typeSymbol(self: Type)(using ctx: Context): Symbol

  def Type_termSymbol(self: Type)(using ctx: Context): Symbol

  def Type_isSingleton(self: Type)(using ctx: Context): Boolean

  def Type_memberType(self: Type)(member: Symbol)(using ctx: Context): Type

  /** The base classes of this type with the class itself as first element. */
  def Type_baseClasses(self: Type)(using ctx: Context): List[Symbol]

  /** The least type instance of given class which is a super-type
    *  of this type.  Example:
    *  {{{
    *    class D[T]
    *    class C extends p.D[Int]
    *    ThisType(C).baseType(D) = p.D[Int]
    * }}}
    */
  def Type_baseType(self: Type)(cls: Symbol)(using ctx: Context): Type

  /** Is this type an instance of a non-bottom subclass of the given class `cls`? */
  def Type_derivesFrom(self: Type)(cls: Symbol)(using ctx: Context): Boolean

  /** Is this type a function type?
   *
   *  @return true if the dealised type of `self` without refinement is `FunctionN[T1, T2, ..., Tn]`
   *
   *  @note The function
   *
   *     - returns true for `given Int => Int` and `erased Int => Int`
   *     - returns false for `List[Int]`, despite that `List[Int] <:< Int => Int`.
   */
  def Type_isFunctionType(self: Type)(using ctx: Context): Boolean

  /** Is this type an context function type?
   *
   *  @see `Type_isFunctionType`
   */
  def Type_isContextFunctionType(self: Type)(using ctx: Context): Boolean

  /** Is this type an erased function type?
   *
   *  @see `Type_isFunctionType`
   */
  def Type_isErasedFunctionType(self: Type)(using ctx: Context): Boolean

  /** Is this type a dependent function type?
   *
   *  @see `Type_isFunctionType`
   */
  def Type_isDependentFunctionType(self: Type)(using ctx: Context): Boolean

  /** The type <this . sym>, reduced if possible */
  def Type_select(self: Type)(sym: Symbol)(using ctx: Context): Type

  def ConstantType_TypeTest: TypeTest[TypeOrBounds, ConstantType]

  def ConstantType_apply(const : Constant)(using ctx : Context) : ConstantType

  def ConstantType_constant(self: ConstantType)(using ctx: Context): Constant

  def TermRef_TypeTest: TypeTest[TypeOrBounds, TermRef]

  def TermRef_apply(qual: TypeOrBounds, name: String)(using ctx: Context): TermRef

  def TermRef_qualifier(self: TermRef)(using ctx: Context): TypeOrBounds
  def TermRef_name(self: TermRef)(using ctx: Context): String

  def TypeRef_TypeTest: TypeTest[TypeOrBounds, TypeRef]

  def TypeRef_qualifier(self: TypeRef)(using ctx: Context): TypeOrBounds
  def TypeRef_name(self: TypeRef)(using ctx: Context): String
  def TypeRef_isOpaqueAlias(self: TypeRef)(using ctx: Context): Boolean
  def TypeRef_translucentSuperType(self: TypeRef)(using ctx: Context): Type

  def SuperType_TypeTest: TypeTest[TypeOrBounds, SuperType]

  def SuperType_apply(thistpe: Type, supertpe: Type)(using ctx: Context): SuperType

  def SuperType_thistpe(self: SuperType)(using ctx: Context): Type
  def SuperType_supertpe(self: SuperType)(using ctx: Context): Type

  def Refinement_TypeTest: TypeTest[TypeOrBounds, Refinement]

  def Refinement_apply(parent: Type, name: String, info: TypeOrBounds /* Type | TypeBounds */)(using ctx: Context): Refinement

  def Refinement_parent(self: Refinement)(using ctx: Context): Type
  def Refinement_name(self: Refinement)(using ctx: Context): String
  def Refinement_info(self: Refinement)(using ctx: Context): TypeOrBounds

  def AppliedType_TypeTest: TypeTest[TypeOrBounds, AppliedType]

  def AppliedType_tycon(self: AppliedType)(using ctx: Context): Type
  def AppliedType_args(self: AppliedType)(using ctx: Context): List[TypeOrBounds]

  def AppliedType_apply(tycon: Type, args: List[TypeOrBounds])(using ctx: Context) : AppliedType

  def AnnotatedType_TypeTest: TypeTest[TypeOrBounds, AnnotatedType]

  def AnnotatedType_apply(underlying: Type, annot: Term)(using ctx: Context): AnnotatedType

  def AnnotatedType_underlying(self: AnnotatedType)(using ctx: Context): Type
  def AnnotatedType_annot(self: AnnotatedType)(using ctx: Context): Term

  def AndType_TypeTest: TypeTest[TypeOrBounds, AndType]

  def AndType_apply(lhs: Type, rhs: Type)(using ctx: Context): AndType

  def AndType_left(self: AndType)(using ctx: Context): Type
  def AndType_right(self: AndType)(using ctx: Context): Type

  def OrType_TypeTest: TypeTest[TypeOrBounds, OrType]

  def OrType_apply(lhs : Type, rhs : Type)(using ctx : Context): OrType

  def OrType_left(self: OrType)(using ctx: Context): Type
  def OrType_right(self: OrType)(using ctx: Context): Type

  def MatchType_TypeTest: TypeTest[TypeOrBounds, MatchType]

  def MatchType_apply(bound: Type, scrutinee: Type, cases: List[Type])(using ctx: Context): MatchType

  def MatchType_bound(self: MatchType)(using ctx: Context): Type
  def MatchType_scrutinee(self: MatchType)(using ctx: Context): Type
  def MatchType_cases(self: MatchType)(using ctx: Context): List[Type]

  def ByNameType_TypeTest: TypeTest[TypeOrBounds, ByNameType]

  def ByNameType_apply(underlying: Type)(using ctx: Context): Type

  def ByNameType_underlying(self: ByNameType)(using ctx: Context): Type

  def ParamRef_TypeTest: TypeTest[TypeOrBounds, ParamRef]

  def ParamRef_binder(self: ParamRef)(using ctx: Context): LambdaType[TypeOrBounds]
  def ParamRef_paramNum(self: ParamRef)(using ctx: Context): Int

  def ThisType_TypeTest: TypeTest[TypeOrBounds, ThisType]

  def ThisType_tref(self: ThisType)(using ctx: Context): Type

  def RecursiveThis_TypeTest: TypeTest[TypeOrBounds, RecursiveThis]

  def RecursiveThis_binder(self: RecursiveThis)(using ctx: Context): RecursiveType

  def RecursiveType_TypeTest: TypeTest[TypeOrBounds, RecursiveType]

  /** Create a RecType, normalizing its contents. This means:
   *
   *   1. Nested Rec types on the type's spine are merged with the outer one.
   *   2. Any refinement of the form `type T = z.T` on the spine of the type
   *      where `z` refers to the created rec-type is replaced by
   *      `type T`. This avoids infinite recursions later when we
   *      try to follow these references.
   */
  def RecursiveType_apply(parentExp: RecursiveType => Type)(using ctx: Context): RecursiveType

  def RecursiveType_underlying(self: RecursiveType)(using ctx: Context): Type

  def RecursiveThis_recThis(self: RecursiveType)(using ctx: Context): RecursiveThis

  def MethodType_TypeTest: TypeTest[TypeOrBounds, MethodType]

  def MethodType_apply(paramNames: List[String])(paramInfosExp: MethodType => List[Type], resultTypeExp: MethodType => Type): MethodType

  def MethodType_isErased(self: MethodType): Boolean
  def MethodType_isImplicit(self: MethodType): Boolean
  def MethodType_param(self: MethodType, ids: Int)(using ctx: Context): Type
  def MethodType_paramNames(self: MethodType)(using ctx: Context): List[String]
  def MethodType_paramTypes(self: MethodType)(using ctx: Context): List[Type]
  def MethodType_resType(self: MethodType)(using ctx: Context): Type

  def PolyType_TypeTest: TypeTest[TypeOrBounds, PolyType]

  def PolyType_apply(paramNames: List[String])(paramBoundsExp: PolyType => List[TypeBounds], resultTypeExp: PolyType => Type)(using ctx: Context): PolyType

  def PolyType_param(self: PolyType, idx: Int)(using ctx: Context): Type
  def PolyType_paramNames(self: PolyType)(using ctx: Context): List[String]
  def PolyType_paramBounds(self: PolyType)(using ctx: Context): List[TypeBounds]
  def PolyType_resType(self: PolyType)(using ctx: Context): Type

  def TypeLambda_TypeTest: TypeTest[TypeOrBounds, TypeLambda]

  def TypeLambda_apply(paramNames: List[String], boundsFn: TypeLambda => List[TypeBounds], bodyFn: TypeLambda => Type): TypeLambda

  def TypeLambda_paramNames(self: TypeLambda)(using ctx: Context): List[String]
  def TypeLambda_paramBounds(self: TypeLambda)(using ctx: Context): List[TypeBounds]
  def TypeLambda_param(self: TypeLambda, idx: Int)(using ctx: Context): Type
  def TypeLambda_resType(self: TypeLambda)(using ctx: Context): Type


  //////////////////////
  // IMPORT SELECTORS //
  //////////////////////

  def SimpleSelector_TypeTest: TypeTest[ImportSelector, SimpleSelector]

  def SimpleSelector_selection(self: SimpleSelector): Id

  def RenameSelector_TypeTest: TypeTest[ImportSelector, RenameSelector]

  def RenameSelector_from(self: RenameSelector): Id
  def RenameSelector_to(self: RenameSelector): Id

  def OmitSelector_TypeTest: TypeTest[ImportSelector, OmitSelector]

  def SimpleSelector_omitted(self: OmitSelector): Id


  /////////////////
  // IDENTIFIERS //
  /////////////////

  /** Position in the source code */
  def Id_pos(self: Id): Position

  /** Name of the identifier */
  def Id_name(self: Id): String


  ////////////////
  // SIGNATURES //
  ////////////////

  def Signature_paramSigs(self: Signature): List[String | Int]

  def Signature_resultSig(self: Signature): String


  ///////////////
  // POSITIONS //
  ///////////////

  /** The start offset in the source file */
  def Position_start(self: Position): Int

  /** The end offset in the source file */
  def Position_end(self: Position): Int

  /** Does this position exist */
  def Position_exists(self: Position): Boolean

  /** Source file in which this position is located */
  def Position_sourceFile(self: Position): SourceFile

  /** The start line in the source file */
  def Position_startLine(self: Position): Int

  /** The end line in the source file */
  def Position_endLine(self: Position): Int

  /** The start column in the source file */
  def Position_startColumn(self: Position): Int

  /** The end column in the source file */
  def Position_endColumn(self: Position): Int

  /** Source code within the position */
  def Position_sourceCode(self: Position): String

  /////////////////
  // SOURCE FILE //
  /////////////////

  /** Path to a source file */
  def SourceFile_jpath(self: SourceFile): java.nio.file.Path

  /** Content of a source file */
  def SourceFile_content(self: SourceFile): String


  //////////////
  // COMMENTS //
  //////////////

  def Comment_raw(self: Comment): String
  def Comment_expanded(self: Comment): Option[String]
  def Comment_usecases(self: Comment): List[(String, Option[DefDef])]


  ///////////////
  // CONSTANTS //
  ///////////////

  def Constant_value(const: Constant): Any

  def matchConstant(constant: Constant): Option[Unit | Null | Int | Boolean | Byte | Short | Int | Long | Float | Double | Char | String | Type]
  def matchConstant_ClassTag(constant: Constant): Option[Type]

  def Constant_apply(x: Unit | Null | Int | Boolean | Byte | Short | Int | Long | Float | Double | Char | String | Type): Constant
  def Constant_ClassTag_apply(x: Type): Constant


  /////////////
  // SYMBOLS //
  /////////////

  /** Returns the symbol of the enclosing definition of the given context */
  def Symbol_currentOwner: Symbol

  /** Owner of this symbol. The owner is the symbol in which this symbol is defined. Throws if this symbol does not have an owner. */
  def Symbol_owner(self: Symbol): Symbol

  /** Owner of this symbol. The owner is the symbol in which this symbol is defined. Returns `NoSymbol` if this symbol does not have an owner. */
  def Symbol_maybeOwner(self: Symbol): Symbol

  /** Flags of this symbol */
  def Symbol_flags(self: Symbol): Flags

  def Symbol_tree(self: Symbol): Tree

  def Symbol_isLocalDummy(self: Symbol): Boolean

  def Symbol_isRefinementClass(self: Symbol): Boolean

  def Symbol_isAliasType(self: Symbol): Boolean

  def Symbol_isAnonymousClass(self: Symbol): Boolean

  def Symbol_isAnonymousFunction(self: Symbol): Boolean

  def Symbol_isAbstractType(self: Symbol): Boolean

  def Symbol_isClassConstructor(self: Symbol): Boolean

  /** This symbol is private within the resulting type. */
  def Symbol_privateWithin(self: Symbol): Option[Type]

  /** This symbol is protected within the resulting type. */
  def Symbol_protectedWithin(self: Symbol): Option[Type]

  /** The name of this symbol. */
  def Symbol_name(self: Symbol): String

  /** The full name of this symbol up to the root package. */
  def Symbol_fullName(self: Symbol): String

  /** The position of this symbol */
  def Symbol_pos(self: Symbol): Position

  def Symbol_localContext(self: Symbol): Context

  /** The comment of the symbol */
  def Symbol_comment(self: Symbol): Option[Comment]

  /** Annotations attached to this symbol */
  def Symbol_annots(self: Symbol): List[Term]

  def Symbol_isDefinedInCurrentRun(self: Symbol): Boolean

  /** Fields directly declared in the class */
  def Symbol_fields(self: Symbol): List[Symbol]

  /** Field with the given name directly declared in the class */
  def Symbol_field(self: Symbol)(name: String): Symbol

  /** Get non-private named methods defined directly inside the class */
  def Symbol_classMethod(self: Symbol)(name: String): List[Symbol]

  /** Get all non-private methods defined directly inside the class, excluding constructors */
  def Symbol_classMethods(self: Symbol): List[Symbol]

  /** Get named non-private methods declared or inherited */
  def Symbol_method(self: Symbol)(name: String): List[Symbol]

  /** Get all non-private methods declared or inherited */
  def Symbol_methods(self: Symbol): List[Symbol]

  /** Type member directly declared in the class */
  def Symbol_typeMembers(self: Symbol): List[Symbol]

  /** Type member with the given name directly declared in the class */
  def Symbol_typeMember(self: Symbol)(name: String): Symbol

  /** The symbols of each type parameter list and value parameter list of this
   *  method, or Nil if this isn't a method.
   */
  def Symbol_paramSymss(self: Symbol): List[List[Symbol]]

  /** The primary constructor of a class or trait, `noSymbol` if not applicable. */
  def Symbol_primaryConstructor(self: Symbol): Symbol

  /** Fields of a case class type -- only the ones declared in primary constructor */
  def Symbol_caseFields(self: Symbol): List[Symbol]

  /** Get package symbol if package is either defined in current compilation run or present on classpath. */
  def Symbol_requiredPackage(path: String): Symbol

  /** Get class symbol if class is either defined in current compilation run or present on classpath. */
  def Symbol_requiredClass(path: String): Symbol

  /** Get module symbol if module is either defined in current compilation run or present on classpath. */
  def Symbol_requiredModule(path: String): Symbol

  /** Get method symbol if method is either defined in current compilation run or present on classpath. Throws if the method has an overload. */
  def Symbol_requiredMethod(path: String): Symbol

  def Symbol_of(fullName: String): Symbol

  def Symbol_newMethod(parent: Symbol, name: String, flags: Flags, tpe: Type, privateWithin: Symbol): Symbol

  def Symbol_newVal(parent: Symbol, name: String, flags: Flags, tpe: Type, privateWithin: Symbol): Symbol

  def Symbol_newBind(parent: Symbol, name: String, flags: Flags, tpe: Type): Symbol

  def Symbol_isTypeParam(self: Symbol): Boolean

  def Symbol_isPackageDef(symbol: Symbol): Boolean

  /** Is this the definition of a type? */
  def Symbol_isType(symbol: Symbol): Boolean

  /** Is this the definition of a term? */
  def Symbol_isTerm(symbol: Symbol): Boolean

  /** Is this the definition of a ClassDef tree? */
  def Symbol_isClassDef(symbol: Symbol): Boolean

  /** Is this the definition of a TypeDef tree? */
  def Symbol_isTypeDef(symbol: Symbol): Boolean

  /** Is this the definition of a DefDef tree? */
  def Symbol_isDefDef(symbol: Symbol): Boolean

  /** Is this the definition of a ValDef tree? */
  def Symbol_isValDef(symbol: Symbol): Boolean

  /** Is this the definition of a Bind pattern? */
  def Symbol_isBind(symbol: Symbol): Boolean

  /** Signature of this definition */
  def Symbol_signature(self: Symbol): Signature

  /** The class symbol of the companion module class */
  def Symbol_moduleClass(self: Symbol): Symbol

  /** The symbol of the companion class */
  def Symbol_companionClass(self: Symbol): Symbol

  /** The symbol of the companion module */
  def Symbol_companionModule(self: Symbol): Symbol

  def Symbol_noSymbol: Symbol

  /** Case class or case object children of a sealed trait */
  def Symbol_children(self: Symbol): List[Symbol]


  ///////////
  // FLAGS //
  ///////////

  /** Is the given flag set a subset of this flag sets */
  def Flags_is(self: Flags)(that: Flags): Boolean

  /** Union of the two flag sets */
  def Flags_or(self: Flags)(that: Flags): Flags

  /** Intersection of the two flag sets */
  def Flags_and(self: Flags)(that: Flags): Flags

  def Flags_Abstract: Flags
  def Flags_Artifact: Flags
  def Flags_Case: Flags
  def Flags_CaseAcessor: Flags
  def Flags_Contravariant: Flags
  def Flags_Covariant: Flags
  def Flags_EmptyFlags: Flags
  def Flags_Enum: Flags
  def Flags_Erased: Flags
  def Flags_ExtensionMethod: Flags
  def Flags_FieldAccessor: Flags
  def Flags_Final: Flags
  def Flags_Given: Flags
  def Flags_HasDefault: Flags
  def Flags_Implicit: Flags
  def Flags_Inline: Flags
  def Flags_JavaDefined: Flags
  def Flags_Lazy: Flags
  def Flags_Local: Flags
  def Flags_Macro: Flags
  def Flags_ModuleClass: Flags
  def Flags_Mutable: Flags
  def Flags_Object: Flags
  def Flags_Override: Flags
  def Flags_Package: Flags
  def Flags_Param: Flags
  def Flags_ParamAccessor: Flags
  def Flags_Private: Flags
  def Flags_PrivateLocal: Flags
  def Flags_Protected: Flags
  def Flags_Scala2X: Flags
  def Flags_Sealed: Flags
  def Flags_StableRealizable: Flags
  def Flags_Static: Flags
  def Flags_Synthetic: Flags
  def Flags_Trait: Flags


  /////////////////
  // DEFINITIONS //
  /////////////////

  def Definitions_RootPackage: Symbol
  def Definitions_RootClass: Symbol

  def Definitions_EmptyPackageClass: Symbol

  def Definitions_ScalaPackage: Symbol
  def Definitions_ScalaPackageClass: Symbol

  def Definitions_AnyClass: Symbol
  def Definitions_AnyValClass: Symbol
  def Definitions_ObjectClass: Symbol
  def Definitions_AnyRefClass: Symbol
  def Definitions_NullClass: Symbol
  def Definitions_NothingClass: Symbol
  def Definitions_UnitClass: Symbol
  def Definitions_ByteClass: Symbol
  def Definitions_ShortClass: Symbol
  def Definitions_CharClass: Symbol
  def Definitions_IntClass: Symbol
  def Definitions_LongClass: Symbol
  def Definitions_FloatClass: Symbol
  def Definitions_DoubleClass: Symbol
  def Definitions_BooleanClass: Symbol
  def Definitions_StringClass: Symbol
  def Definitions_ClassClass: Symbol
  def Definitions_ArrayClass: Symbol
  def Definitions_PredefModule: Symbol
  def Definitions_Predef_classOf: Symbol

  def Definitions_JavaLangPackage: Symbol

  def Definitions_ArrayModule: Symbol

  def Definitions_Array_apply: Symbol
  def Definitions_Array_clone: Symbol
  def Definitions_Array_length: Symbol
  def Definitions_Array_update: Symbol

  /** A dummy class symbol that is used to indicate repeated parameters
   *  compiled by the Scala compiler.
   */
  def Definitions_RepeatedParamClass: Symbol

  /** The class symbol of class `scala.annotation.internal.Repeated` */
  def Definitions_RepeatedAnnot: Symbol

  def Definitions_OptionClass: Symbol
  def Definitions_NoneModule: Symbol
  def Definitions_SomeModule: Symbol

  def Definitions_ProductClass: Symbol
  // TODO avoid default parameters
  def Definitions_FunctionClass(arity: Int, isImplicit: Boolean = false, isErased: Boolean = false): Symbol

  def Definitions_TupleClass(arity: Int): Symbol
  def Definitions_isTupleClass(sym: Symbol): Boolean

  /** Symbol of scala.internal.CompileTime.patternHole */
  def Definitions_InternalQuotedMatcher_patternHole: Symbol

  /** Symbol of scala.internal.CompileTime.higherOrderHole */
  def Definitions_InternalQuotedMatcher_higherOrderHole: Symbol

  /** Symbol of scala.internal.CompileTime.patternType */
  def Definitions_InternalQuotedMatcher_patternTypeAnnot: Symbol

  /** Symbol of scala.internal.CompileTime.fromAbove */
  def Definitions_InternalQuotedMatcher_fromAboveAnnot: Symbol

  /** The type of primitive type `Unit`. */
  def Definitions_UnitType: Type

  /** The type of primitive type `Byte`. */
  def Definitions_ByteType: Type

  /** The type of primitive type `Short`. */
  def Definitions_ShortType: Type

  /** The type of primitive type `Char`. */
  def Definitions_CharType: Type

  /** The type of primitive type `Int`. */
  def Definitions_IntType: Type

  /** The type of primitive type `Long`. */
  def Definitions_LongType: Type

  /** The type of primitive type `Float`. */
  def Definitions_FloatType: Type

  /** The type of primitive type `Double`. */
  def Definitions_DoubleType: Type

  /** The type of primitive type `Boolean`. */
  def Definitions_BooleanType: Type

  /** The type of core type `Any`. */
  def Definitions_AnyType: Type

  /** The type of core type `AnyVal`. */
  def Definitions_AnyValType: Type

  /** The type of core type `AnyRef`. */
  def Definitions_AnyRefType: Type

  /** The type of core type `Object`. */
  def Definitions_ObjectType: Type

  /** The type of core type `Nothing`. */
  def Definitions_NothingType: Type

  /** The type of core type `Null`. */
  def Definitions_NullType: Type

  /** The type for `scala.String`. */
  def Definitions_StringType: Type

  /** The type for `scala.Tuple`. */
  def Definitions_TupleType: Type

  /** The type for `scala.EmptyTuple`. */
  def Definitions_EmptyTupleType: Type

  /** The type for `scala.NonEmptyTuple`. */
  def Definitions_NonEmptyTupleType: Type

  /** The type for `scala.*:`. */
  def Definitions_TupleConsType: Type

  ///////////////
  // IMPLICITS //
  ///////////////

  def ImplicitSearchSuccess_TypeTest: TypeTest[ImplicitSearchResult, ImplicitSearchSuccess]
  def ImplicitSearchSuccess_tree(self: ImplicitSearchSuccess): Term

  def ImplicitSearchFailure_TypeTest: TypeTest[ImplicitSearchResult, ImplicitSearchFailure]
  def ImplicitSearchFailure_explanation(self: ImplicitSearchFailure): String

  def DivergingImplicit_TypeTest: TypeTest[ImplicitSearchResult, DivergingImplicit]

  def NoMatchingImplicits_TypeTest: TypeTest[ImplicitSearchResult, NoMatchingImplicits]

  def AmbiguousImplicits_TypeTest: TypeTest[ImplicitSearchResult, AmbiguousImplicits]

  /** Find an implicit of type `T` in the current scope given by `ctx`.
   *  Return an `ImplicitSearchResult`.
   *
   *  @param tpe type of the implicit parameter
   *  @param ctx current context
   */
  def searchImplicit(tpe: Type): ImplicitSearchResult

  /** Returns Some with a beta-reduced application or None */
  def betaReduce(tree: Term): Option[Term]

  def lambdaExtractor(term: Term, paramTypes: List[Type]): Option[List[Term] => Term]

  def compilerId: Int

}


object CompilerInterface {

  private[scala] def quoteContextWithCompilerInterface(qctx: QuoteContext): qctx.type { val tasty: qctx.tasty.type & scala.internal.tasty.CompilerInterface } =
    qctx.asInstanceOf[qctx.type { val tasty: qctx.tasty.type & scala.internal.tasty.CompilerInterface }]

}
