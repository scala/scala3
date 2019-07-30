package scala.tasty.reflect

import scala.quoted.QuoteContext

/** Tasty reflect abstract types
 *
 *  ```none
 *
 *  +- Tree -+- PackageClause
 *           +- Import
 *           +- Statement -+- Definition --+- PackageDef
 *           |             |               +- ClassDef
 *           |             |               +- TypeDef
 *           |             |               +- DefDef
 *           |             |               +- ValDef
 *           |             |
 *           |             +- Term --------+- Ref -+- Ident
 *           |                             |       +- Select
 *           |                             |
 *           |                             +- Literal
 *           |                             +- This
 *           |                             +- New
 *           |                             +- NamedArg
 *           |                             +- Apply
 *           |                             +- TypeApply
 *           |                             +- Super
 *           |                             +- Typed
 *           |                             +- Assign
 *           |                             +- Block
 *           |                             +- Closure
 *           |                             +- If
 *           |                             +- Match
 *           |                             +- ImpliedMatch
 *           |                             +- Try
 *           |                             +- Return
 *           |                             +- Repeated
 *           |                             +- Inlined
 *           |                             +- SelectOuter
 *           |                             +- While
 *           |
 *           |
 *           +- TypeTree ----+- Inferred
 *           |               +- TypeIdent
 *           |               +- TypeSelect
 *           |               +- Projection
 *           |               +- Singleton
 *           |               +- Refined
 *           |               +- Applied
 *           |               +- Annotated
 *           |               +- MatchTypeTree
 *           |               +- ByName
 *           |               +- LambdaTypeTree
 *           |               +- TypeBind
 *           |               +- TypeBlock
 *           |
 *           +- TypeBoundsTree
 *           +- WildcardTypeTree
 *           +- CaseDef
 *           +- TypeCaseDef
 *
 *  +- Pattern --+- Value
 *               +- Bind
 *               +- Unapply
 *               +- Alternatives
 *               +- TypeTest
 *               +- WildcardPattern
 *
 *
 *                   +- NoPrefix
 *  +- TypeOrBounds -+- TypeBounds
 *                   |
 *                   +- Type -------+- ConstantType
 *                                  +- SymRef
 *                                  +- TermRef
 *                                  +- TypeRef
 *                                  +- SuperType
 *                                  +- Refinement
 *                                  +- AppliedType
 *                                  +- AnnotatedType
 *                                  +- AndType
 *                                  +- OrType
 *                                  +- MatchType
 *                                  +- ByNameType
 *                                  +- ParamRef
 *                                  +- ThisType
 *                                  +- RecursiveThis
 *                                  +- RecursiveType
 *                                  +- LambdaType[ParamInfo <: TypeOrBounds] -+- MethodType
 *                                                                            +- PolyType
 *                                                                            +- TypeLambda
 *
 *  +- ImportSelector -+- SimpleSelector
 *                     +- RenameSelector
 *                     +- OmitSelector
 *
 *  +- Id
 *
 *  +- Signature
 *
 *  +- Position
 *
 *  +- Comment
 *
 *  +- Constant
 *
 *  +- Symbol --+- PackageDefSymbol
 *              |
 *              +- TypeSymbol -+- ClassDefSymbol
 *              |              +- TypeDefSymbol
 *              |              +- TypeBindSymbol
 *              |
 *              +- TermSymbol -+- DefDefSymbol
 *              |              +- ValDefSymbol
 *              |              +- BindSymbol
 *              |
 *              +- NoSymbol
 *
 *  +- Flags
 *
 *  ```
 */
trait Kernel {

  /** Context of the macro expansion */
  def rootContext: Context

  /** Root position of this tasty context. For macros it corresponds to the expansion site. */
  def rootPosition: Position

  def settings: Settings

  //
  // CONTEXT
  //

  /** Compilation context */
  type Context <: AnyRef

  /** Returns the owner of the context */
  def Context_owner(self: Context): Symbol

  /** Returns the source file being compiled. The path is relative to the current working directory. */
  def Context_source(self: Context): java.nio.file.Path

  def Context_GADT_setFreshGADTBounds(self: Context): Context
  def Context_GADT_addToConstraint(self: Context)(syms: List[Symbol]): Boolean
  def Context_GADT_approximation(self: Context)(sym: Symbol, fromBelow: Boolean): Type

  //
  // REPORTING
  //

  /** Report a compilation error with the given message at the given position */
  def error(msg: => String, pos: Position) given Context: Unit

  /** Report a compilation error with the given message at the given position range */
  def error(msg: => String, source: SourceFile, start: Int, end: Int) given Context: Unit

  /** Report a compilation warning with the given message at the given position */
  def warning(msg: => String, pos: Position) given Context: Unit

  /** Report a compilation warning with the given message at the given position range */
  def warning(msg: => String, source: SourceFile, start: Int, end: Int) given Context: Unit

  //
  // Settings
  //

  /** Settings */
  type Settings <: AnyRef

  def Settings_color(self: Settings): Boolean

  //
  // MISC
  //
  /** Whether the code type checks in the given context?
   *
   *  @param code The code to be type checked
   *
   *  The code should be a sequence of expressions or statements that may appear in a block.
   */
  def typeChecks(code: String) given Context: Boolean

  //
  // TREES
  //

  /** Tree representing code written in the source */
  type Tree <: AnyRef

  def Tree_pos(self: Tree) given Context: Position
  def Tree_symbol(self: Tree) given Context: Symbol

  /** Tree representing a pacakage clause in the source code */
  type PackageClause <: Tree

  def matchPackageClause(tree: Tree) given Context: Option[PackageClause]

  def PackageClause_pid(self: PackageClause) given Context: Ref
  def PackageClause_stats(self: PackageClause) given Context: List[Tree]

  def PackageClause_apply(pid: Ref, stats: List[Tree]) given Context: PackageClause

  def PackageClause_copy(original: PackageClause)(pid: Ref, stats: List[Tree]) given Context: PackageClause

  /** Tree representing a statement in the source code */
  type Statement <: Tree

  def matchStatement(tree: Tree) given Context: Option[Statement]

  /** Tree representing an import in the source code */
  type Import <: Statement

  def matchImport(tree: Tree) given Context: Option[Import]

  def Import_implied(self: Import): Boolean
  def Import_expr(self: Import) given Context: Term
  def Import_selectors(self: Import) given Context: List[ImportSelector]

  def Import_apply(importImplied: Boolean, expr: Term, selectors: List[ImportSelector]) given Context: Import

  def Import_copy(original: Import)(importImplied: Boolean, expr: Term, selectors: List[ImportSelector]) given Context: Import

  /** Tree representing a definition in the source code. It can be `PackageDef`, `ClassDef`, `TypeDef`, `DefDef` or `ValDef` */
  type Definition <: Statement

  def matchDefinition(tree: Tree) given Context: Option[Definition]

  def Definition_name(self: Definition) given Context: String

  /** Tree representing a package definition. This includes definitions in all source files */
  type PackageDef <: Definition

  def matchPackageDef(tree: Tree) given Context: Option[PackageDef]

  def PackageDef_owner(self: PackageDef) given Context: PackageDef
  def PackageDef_members(self: PackageDef) given Context: List[Statement]
  def PackageDef_symbol(self: PackageDef) given Context: PackageDefSymbol

  /** Tree representing a class definition. This includes annonymus class definitions and the class of a module object */
  type ClassDef <: Definition

  def matchClassDef(tree: Tree) given Context: Option[ClassDef]

  def ClassDef_constructor(self: ClassDef) given Context: DefDef
  def ClassDef_parents(self: ClassDef) given Context: List[Tree/* Term | TypeTree */]
  def ClassDef_derived(self: ClassDef) given Context: List[TypeTree]
  def ClassDef_self(self: ClassDef) given Context: Option[ValDef]
  def ClassDef_body(self: ClassDef) given Context: List[Statement]
  def ClassDef_symbol(self: ClassDef) given Context: ClassDefSymbol

  def ClassDef_copy(original: ClassDef)(name: String, constr: DefDef, parents: List[Tree/* Term | TypeTree */], derived: List[TypeTree], selfOpt: Option[ValDef], body: List[Statement]) given Context: ClassDef

  /** Tree representing a type (paramter or member) definition in the source code */
  type TypeDef <: Definition

  def matchTypeDef(tree: Tree) given Context: Option[TypeDef]

  def TypeDef_rhs(self: TypeDef) given Context: Tree /*TypeTree | TypeBoundsTree*/
  def TypeDef_symbol(self: TypeDef) given Context: TypeDefSymbol

  def TypeDef_apply(symbol: TypeDefSymbol) given Context: TypeDef
  def TypeDef_copy(original: TypeDef)(name: String, rhs: Tree /*TypeTree | TypeBoundsTree*/) given Context: TypeDef

  /** Tree representing a method definition in the source code */
  type DefDef <: Definition

  def matchDefDef(tree: Tree) given Context: Option[DefDef]

  def DefDef_typeParams(self: DefDef) given Context: List[TypeDef]
  def DefDef_paramss(self: DefDef) given Context: List[List[ValDef]]
  def DefDef_returnTpt(self: DefDef) given Context: TypeTree
  def DefDef_rhs(self: DefDef) given Context: Option[Term]
  def DefDef_symbol(self: DefDef) given Context: DefDefSymbol

  def DefDef_apply(symbol: DefDefSymbol, rhsFn: List[Type] => List[List[Term]] => Option[Term]) given Context: DefDef
  def DefDef_copy(original: DefDef)(name: String, typeParams: List[TypeDef], paramss: List[List[ValDef]], tpt: TypeTree, rhs: Option[Term]) given Context: DefDef

  /** Tree representing a value definition in the source code This inclues `val`, `lazy val`, `var`, `object` and parameter definitions. */
  type ValDef <: Definition

  def matchValDef(tree: Tree) given Context: Option[ValDef]

  def ValDef_tpt(self: ValDef) given Context: TypeTree
  def ValDef_rhs(self: ValDef) given Context: Option[Term]
  def ValDef_symbol(self: ValDef) given Context: ValDefSymbol

  def ValDef_apply(symbol: ValDefSymbol, rhs: Option[Term]) given Context: ValDef
  def ValDef_copy(original: ValDef)(name: String, tpt: TypeTree, rhs: Option[Term]) given Context: ValDef

  /** Tree representing an expression in the source code */
  type Term <: Statement

  def matchTerm(tree: Tree) given Context: Option[Term]

  def Term_pos(self: Term) given Context: Position
  def Term_tpe(self: Term) given Context: Type
  def Term_underlyingArgument(self: Term) given Context: Term
  def Term_underlying(self: Term) given Context: Term

  /** Tree representing a reference to definition */
  type Ref <: Term

  def matchRef(tree: Tree) given Context: Option[Ref]

  def Ref_apply(sym: Symbol) given Context: Ref

  /** Tree representing a reference to definition with a given name */
  type Ident <: Ref

  def matchIdent(tree: Tree) given Context: Option[Ident]

  def Ident_name(self: Ident) given Context: String

  def Ident_apply(tmref: TermRef) given Context: Term
  def Ident_copy(original: Tree)(name: String) given Context: Ident

  /** Tree representing a selection of definition with a given name on a given prefix */
  type Select <: Ref

  def matchSelect(tree: Tree) given Context: Option[Select]

  def Select_qualifier(self: Select) given Context: Term
  def Select_name(self: Select) given Context: String
  def Select_signature(self: Select) given Context: Option[Signature]

  def Select_apply(qualifier: Term, symbol: Symbol) given Context: Select
  def Select_unique(qualifier: Term, name: String) given Context: Select
  // TODO rename, this returns an Apply and not a Select
  def Select_overloaded(qualifier: Term, name: String, targs: List[Type], args: List[Term]) given Context: Apply
  def Select_copy(original: Tree)(qualifier: Term, name: String) given Context: Select

  /** Tree representing a literal value in the source code */
  type Literal <: Term

  def matchLiteral(tree: Tree) given Context: Option[Literal]

  def Literal_constant(self: Literal) given Context: Constant

  def Literal_apply(constant: Constant) given Context: Literal
  def Literal_copy(original: Tree)(constant: Constant) given Context: Literal

  /** Tree representing `this` in the source code */
  type This <: Term

  def matchThis(tree: Tree) given Context: Option[This]

  def This_id(self: This) given Context: Option[Id]

  def This_apply(cls: ClassDefSymbol) given Context: This
  def This_copy(original: Tree)(qual: Option[Id]) given Context: This

  /** Tree representing `new` in the source code */
  type New <: Term

  def matchNew(tree: Tree) given Context: Option[New]

  def New_tpt(self: New) given Context: TypeTree

  def New_apply(tpt: TypeTree) given Context: New
  def New_copy(original: Tree)(tpt: TypeTree) given Context: New

  /** Tree representing an argument passed with an explicit name. Such as `arg1 = x` in `foo(arg1 = x)` */
  type NamedArg <: Term

  def matchNamedArg(tree: Tree) given Context: Option[NamedArg]

  def NamedArg_name(self: NamedArg) given Context: String
  def NamedArg_value(self: NamedArg) given Context: Term

  def NamedArg_apply(name: String, arg: Term) given Context: NamedArg
  def NamedArg_copy(tree: NamedArg)(name: String, arg: Term) given Context: NamedArg

  /** Tree an application of arguments. It represents a single list of arguments, multiple argument lists will have nested `Apply`s */
  type Apply <: Term

  def matchApply(tree: Tree) given Context: Option[Apply]

  def Apply_fun(self: Apply) given Context: Term
  def Apply_args(self: Apply) given Context: List[Term]

  def Apply_apply(fn: Term, args: List[Term]) given Context: Apply
  def Apply_copy(original: Tree)(fun: Term, args: List[Term]) given Context: Apply

  /** Tree an application of type arguments */
  type TypeApply <: Term

  def matchTypeApply(tree: Tree) given Context: Option[TypeApply]

  def TypeApply_fun(self: TypeApply) given Context: Term
  def TypeApply_args(self: TypeApply) given Context: List[TypeTree]

  def TypeApply_apply(fn: Term, args: List[TypeTree]) given Context: TypeApply
  def TypeApply_copy(original: Tree)(fun: Term, args: List[TypeTree]) given Context: TypeApply

  /** Tree representing `super` in the source code */
  type Super <: Term

  def matchSuper(tree: Tree) given Context: Option[Super]

  def Super_qualifier(self: Super) given Context: Term
  def Super_id(self: Super) given Context: Option[Id]

  def Super_apply(qual: Term, mix: Option[Id]) given Context: Super
  def Super_copy(original: Tree)(qual: Term, mix: Option[Id]) given Context: Super

  /** Tree representing a type ascription `x: T` in the source code */
  type Typed <: Term

  def matchTyped(tree: Tree) given Context: Option[Typed]

  def Typed_expr(self: Typed) given Context: Term
  def Typed_tpt(self: Typed) given Context: TypeTree

  def Typed_apply(expr: Term, tpt: TypeTree) given Context: Typed
  def Typed_copy(original: Tree)(expr: Term, tpt: TypeTree) given Context: Typed

  /** Tree representing an assignment `x = y` in the source code */
  type Assign <: Term

  def matchAssign(tree: Tree) given Context: Option[Assign]

  def Assign_lhs(self: Assign) given Context: Term
  def Assign_rhs(self: Assign) given Context: Term

  def Assign_apply(lhs: Term, rhs: Term) given Context: Assign
  def Assign_copy(original: Tree)(lhs: Term, rhs: Term) given Context: Assign

  /** Tree representing a block `{ ... }` in the source code */
  type Block <: Term

  def matchBlock(tree: Tree) given Context: Option[Block]

  def Block_statements(self: Block) given Context: List[Statement]
  def Block_expr(self: Block) given Context: Term

  def Block_apply(stats: List[Statement], expr: Term) given Context: Block
  def Block_copy(original: Tree)(stats: List[Statement], expr: Term) given Context: Block

  /** A lambda `(...) => ...` in the source code is represented as
   *  a local method and a closure:
   *
   *  {
   *    def m(...) = ...
   *    closure(m)
   *  }
   *
   */
  type Closure <: Term

  def matchClosure(tree: Tree) given Context: Option[Closure]

  def Closure_meth(self: Closure) given Context: Term
  def Closure_tpeOpt(self: Closure) given Context: Option[Type]

  def Closure_apply(meth: Term, tpe: Option[Type]) given Context: Closure
  def Closure_copy(original: Tree)(meth: Tree, tpe: Option[Type]) given Context: Closure

  /** Tree representing an if/then/else `if (...) ... else ...` in the source code */
  type If <: Term

  def matchIf(tree: Tree) given Context: Option[If]

  def If_cond(self: If) given Context: Term
  def If_thenp(self: If) given Context: Term
  def If_elsep(self: If) given Context: Term

  def If_apply(cond: Term, thenp: Term, elsep: Term) given Context: If
  def If_copy(original: Tree)(cond: Term, thenp: Term, elsep: Term) given Context: If

  /** Tree representing a pattern match `x match  { ... }` in the source code */
  type Match <: Term

  def matchMatch(tree: Tree) given Context: Option[Match]

  def Match_scrutinee(self: Match) given Context: Term
  def Match_cases(self: Match) given Context: List[CaseDef]

  def Match_apply(selector: Term, cases: List[CaseDef]) given Context: Match
  def Match_copy(original: Tree)(selector: Term, cases: List[CaseDef]) given Context: Match

  /** Tree representing a pattern match `delegate match  { ... }` in the source code */
  type ImpliedMatch <: Term

  def matchImplicitMatch(tree: Tree) given Context: Option[ImpliedMatch]

  def ImplicitMatch_cases(self: ImpliedMatch) given Context: List[CaseDef]

  def ImplicitMatch_apply(cases: List[CaseDef]) given Context: ImpliedMatch
  def ImplicitMatch_copy(original: Tree)(cases: List[CaseDef]) given Context: ImpliedMatch

  /** Tree representing a tyr catch `try x catch { ... } finally { ... }` in the source code */
  type Try <: Term

  def matchTry(tree: Tree) given Context: Option[Try]

  def Try_body(self: Try) given Context: Term
  def Try_cases(self: Try) given Context: List[CaseDef]
  def Try_finalizer(self: Try) given Context: Option[Term]

  def Try_apply(expr: Term, cases: List[CaseDef], finalizer: Option[Term]) given Context: Try
  def Try_copy(original: Tree)(expr: Term, cases: List[CaseDef], finalizer: Option[Term]) given Context: Try

  /** Tree representing a `return` in the source code */
  type Return <: Term

  def matchReturn(tree: Tree) given Context: Option[Return]

  def Return_expr(self: Return) given Context: Term

  def Return_apply(expr: Term) given Context: Return
  def Return_copy(original: Tree)(expr: Term) given Context: Return

  /** Tree representing a variable argument list in the source code */
  type Repeated <: Term

  def matchRepeated(tree: Tree) given Context: Option[Repeated]

  def Repeated_elems(self: Repeated) given Context: List[Term]
  def Repeated_elemtpt(self: Repeated) given Context: TypeTree

  def Repeated_apply(elems: List[Term], elemtpt: TypeTree) given Context: Repeated
  def Repeated_copy(original: Tree)(elems: List[Term], elemtpt: TypeTree) given Context: Repeated

  /** Tree representing the scope of an inlined tree */
  type Inlined <: Term

  def matchInlined(tree: Tree) given Context: Option[Inlined]

  def Inlined_call(self: Inlined) given Context: Option[Tree/* Term | TypeTree */]
  def Inlined_bindings(self: Inlined) given Context: List[Definition]
  def Inlined_body(self: Inlined) given Context: Term

  def Inlined_apply(call: Option[Tree/* Term | TypeTree */], bindings: List[Definition], expansion: Term) given Context: Inlined
  def Inlined_copy(original: Tree)(call: Option[Tree/* Term | TypeTree */], bindings: List[Definition], expansion: Term) given Context: Inlined

  /** Tree representing a selection of definition with a given name on a given prefix and number of nested scopes of inlined trees */
  type SelectOuter <: Term

  def matchSelectOuter(tree: Tree) given Context: Option[SelectOuter]

  def SelectOuter_qualifier(self: SelectOuter) given Context: Term
  def SelectOuter_level(self: SelectOuter) given Context: Int
  def SelectOuter_tpe(self: SelectOuter) given Context: Type

  def SelectOuter_apply(qualifier: Term, name: String, levels: Int) given Context: SelectOuter
  def SelectOuter_copy(original: Tree)(qualifier: Term, name: String, levels: Int) given Context: SelectOuter

  /** Tree representing a while loop */
  type While <: Term

  def matchWhile(tree: Tree) given Context: Option[While]

  def While_cond(self: While) given Context: Term
  def While_body(self: While) given Context: Term

  def While_apply(cond: Term, body: Term) given Context: While
  def While_copy(original: Tree)(cond: Term, body: Term) given Context: While

  /** Type tree representing a type written in the source */
  type TypeTree <: Tree

  def matchTypeTree(tree: Tree) given Context: Option[TypeTree]

  def TypeTree_pos(self: TypeTree) given Context: Position
  def TypeTree_symbol(self: TypeTree) given Context: Symbol
  def TypeTree_tpe(self: TypeTree) given Context: Type

  /** Type tree representing an inferred type */
  type Inferred <: TypeTree

  def matchInferred(tree: Tree) given Context: Option[Inferred]

  def Inferred_apply(tpe: Type) given Context: Inferred

  /** Type tree representing a reference to definition with a given name */
  type TypeIdent <: TypeTree

  def matchTypeIdent(tree: Tree) given Context: Option[TypeIdent]

  def TypeIdent_name(self: TypeIdent) given Context: String

  def TypeIdent_copy(original: TypeIdent)(name: String) given Context: TypeIdent

  /** Type tree representing a selection of definition with a given name on a given term prefix */
  type TypeSelect <: TypeTree

  def matchTypeSelect(tree: Tree) given Context: Option[TypeSelect]

  def TypeSelect_qualifier(self: TypeSelect) given Context: Term
  def TypeSelect_name(self: TypeSelect) given Context: String

  def TypeSelect_apply(qualifier: Term, name: String) given Context: TypeSelect
  def TypeSelect_copy(original: TypeSelect)(qualifier: Term, name: String) given Context: TypeSelect

  /** Type tree representing a selection of definition with a given name on a given type prefix */
  type Projection <: TypeTree

  def matchProjection(tree: Tree) given Context: Option[Projection]

  def Projection_qualifier(self: Projection) given Context: TypeTree
  def Projection_name(self: Projection) given Context: String

  def Projection_copy(original: Projection)(qualifier: TypeTree, name: String) given Context: Projection

  /** Type tree representing a singleton type */
  type Singleton <: TypeTree

  def matchSingleton(tree: Tree) given Context: Option[Singleton]

  def Singleton_ref(self: Singleton) given Context: Term

  def Singleton_apply(ref: Term) given Context: Singleton
  def Singleton_copy(original: Singleton)(ref: Term) given Context: Singleton

  /** Type tree representing a type refinement */
  type Refined <: TypeTree

  def matchRefined(tree: Tree) given Context: Option[Refined]

  def Refined_tpt(self: Refined) given Context: TypeTree
  def Refined_refinements(self: Refined) given Context: List[Definition]

  def Refined_copy(original: Refined)(tpt: TypeTree, refinements: List[Definition]) given Context: Refined

  /** Type tree representing a type application */
  type Applied <: TypeTree

  def matchApplied(tree: Tree) given Context: Option[Applied]

  def Applied_tpt(self: Applied) given Context: TypeTree
  def Applied_args(self: Applied) given Context: List[Tree /*TypeTree | TypeBoundsTree*/]

  def Applied_apply(tpt: TypeTree, args: List[Tree /*TypeTree | TypeBoundsTree*/]) given Context: Applied
  def Applied_copy(original: Applied)(tpt: TypeTree, args: List[Tree /*TypeTree | TypeBoundsTree*/]) given Context: Applied

  /** Type tree representing an annotated type */
  type Annotated <: TypeTree

  def matchAnnotated(tree: Tree) given Context: Option[Annotated]

  def Annotated_arg(self: Annotated) given Context: TypeTree
  def Annotated_annotation(self: Annotated) given Context: Term

  def Annotated_apply(arg: TypeTree, annotation: Term) given Context: Annotated
  def Annotated_copy(original: Annotated)(arg: TypeTree, annotation: Term) given Context: Annotated

  /** Type tree representing a type match */
  type MatchTypeTree <: TypeTree

  def matchMatchTypeTree(tree: Tree) given Context: Option[MatchTypeTree]

  def MatchTypeTree_bound(self: MatchTypeTree) given Context: Option[TypeTree]
  def MatchTypeTree_selector(self: MatchTypeTree) given Context: TypeTree
  def MatchTypeTree_cases(self: MatchTypeTree) given Context: List[TypeCaseDef]

  def MatchTypeTree_apply(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef]) given Context: MatchTypeTree
  def MatchTypeTree_copy(original: MatchTypeTree)(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef]) given Context: MatchTypeTree

  /** Type tree representing a by name parameter */
  type ByName <: TypeTree

  def ByName_result(self: ByName) given Context: TypeTree

  def matchByName(tree: Tree) given Context: Option[ByName]

  def ByName_apply(result: TypeTree) given Context: ByName
  def ByName_copy(original: ByName)(result: TypeTree) given Context: ByName

  /** Type tree representing a lambda abstraction type */
  type LambdaTypeTree <: TypeTree

  def matchLambdaTypeTree(tree: Tree) given Context: Option[LambdaTypeTree]

  def Lambdatparams(self: LambdaTypeTree) given Context: List[TypeDef]
  def Lambdabody(self: LambdaTypeTree) given Context: Tree /*TypeTree | TypeBoundsTree*/

  def Lambdaapply(tparams: List[TypeDef], body: Tree /*TypeTree | TypeBoundsTree*/) given Context: LambdaTypeTree
  def Lambdacopy(original: LambdaTypeTree)(tparams: List[TypeDef], body: Tree /*TypeTree | TypeBoundsTree*/) given Context: LambdaTypeTree

  /** Type tree representing a type binding */
  type TypeBind <: TypeTree

  def matchTypeBind(tree: Tree) given Context: Option[TypeBind]

  def TypeBind_name(self: TypeBind) given Context: String
  def TypeBind_body(self: TypeBind) given Context: Tree /*TypeTree | TypeBoundsTree*/

  def TypeBind_copy(original: TypeBind)(name: String, tpt: Tree /*TypeTree | TypeBoundsTree*/) given Context: TypeBind

  /** Type tree within a block with aliases `{ type U1 = ... ; T[U1, U2] }` */
  type TypeBlock <: TypeTree

  def matchTypeBlock(tree: Tree) given Context: Option[TypeBlock]

  def TypeBlock_aliases(self: TypeBlock) given Context: List[TypeDef]
  def TypeBlock_tpt(self: TypeBlock) given Context: TypeTree

  def TypeBlock_apply(aliases: List[TypeDef], tpt: TypeTree) given Context: TypeBlock
  def TypeBlock_copy(original: TypeBlock)(aliases: List[TypeDef], tpt: TypeTree) given Context: TypeBlock

  /** Type tree representing a type bound written in the source */
  type TypeBoundsTree <: Tree /*TypeTree | TypeBoundsTree*/

  def matchTypeBoundsTree(tree: Tree) given Context: Option[TypeBoundsTree]

  def TypeBoundsTree_tpe(self: TypeBoundsTree) given Context: TypeBounds
  def TypeBoundsTree_low(self: TypeBoundsTree) given Context: TypeTree
  def TypeBoundsTree_hi(self: TypeBoundsTree) given Context: TypeTree

  /** Type tree representing wildcard type bounds written in the source.
    *  The wildcard type `_` (for example in in `List[_]`) will be a type tree that
    *  represents a type but has `TypeBound`a inside.
    */
  type WildcardTypeTree <: Tree

  def matchWildcardTypeTree(tree: Tree) given Context: Option[WildcardTypeTree]

  def WildcardTypeTree_tpe(self: WildcardTypeTree) given Context: TypeOrBounds

  /** Branch of a pattern match or catch clause */
  type CaseDef <: Tree

  def matchCaseDef(tree: Tree) given Context: Option[CaseDef]

  def CaseDef_pattern(self: CaseDef) given Context: Pattern
  def CaseDef_guard(self: CaseDef) given Context: Option[Term]
  def CaseDef_rhs(self: CaseDef) given Context: Term

  def CaseDef_module_apply(pattern: Pattern, guard: Option[Term], body: Term) given Context: CaseDef
  def CaseDef_module_copy(original: CaseDef)(pattern: Pattern, guard: Option[Term], body: Term) given Context: CaseDef

  /** Branch of a type pattern match */
  type TypeCaseDef <: Tree

  def matchTypeCaseDef(tree: Tree) given Context: Option[TypeCaseDef]

  def TypeCaseDef_pattern(self: TypeCaseDef) given Context: TypeTree
  def TypeCaseDef_rhs(self: TypeCaseDef) given Context: TypeTree

  def TypeCaseDef_module_apply(pattern: TypeTree, body: TypeTree) given Context: TypeCaseDef
  def TypeCaseDef_module_copy(original: TypeCaseDef)(pattern: TypeTree, body: TypeTree) given Context: TypeCaseDef

  //
  // PATTERNS
  //

  /** Pattern tree of the pattern part of a CaseDef */
  type Pattern <: AnyRef

  def Pattern_pos(self: Pattern) given Context: Position
  def Pattern_tpe(self: Pattern) given Context: Type
  def Pattern_symbol(self: Pattern) given Context: Symbol

  /** Pattern representing a value. This includes `1`, ```x``` and `_` */
  type Value <: Pattern

  def matchPattern_Value(pattern: Pattern): Option[Value]

  def Pattern_Value_value(self: Value) given Context: Term

  def Pattern_Value_module_apply(term: Term) given Context: Value
  def Pattern_Value_module_copy(original: Value)(term: Term) given Context: Value

  /** Pattern representing a `_ @ _` binding. */
  type Bind <: Pattern

  def matchPattern_Bind(x: Pattern) given Context: Option[Bind]

  def Pattern_Bind_name(self: Bind) given Context: String

  def Pattern_Bind_pattern(self: Bind) given Context: Pattern

  def Pattern_Bind_module_copy(original: Bind)(name: String, pattern: Pattern) given Context: Bind

  /** Pattern representing a `Xyz(...)` unapply. */
  type Unapply <: Pattern

  def matchPattern_Unapply(pattern: Pattern) given Context: Option[Unapply]

  def Pattern_Unapply_fun(self: Unapply) given Context: Term

  def Pattern_Unapply_implicits(self: Unapply) given Context: List[Term]

  def Pattern_Unapply_patterns(self: Unapply) given Context: List[Pattern]

  def Pattern_Unapply_module_copy(original: Unapply)(fun: Term, implicits: List[Term], patterns: List[Pattern]) given Context: Unapply

  /** Pattern representing `X | Y | ...` alternatives. */
  type Alternatives <: Pattern

  def matchPattern_Alternatives(pattern: Pattern) given Context: Option[Alternatives]

  def Pattern_Alternatives_patterns(self: Alternatives) given Context: List[Pattern]

  def Pattern_Alternatives_module_apply(patterns: List[Pattern]) given Context: Alternatives
  def Pattern_Alternatives_module_copy(original: Alternatives)(patterns: List[Pattern]) given Context: Alternatives

  /** Pattern representing a `x: Y` type test. */
  type TypeTest <: Pattern

  def matchPattern_TypeTest(pattern: Pattern) given Context: Option[TypeTest]

  def Pattern_TypeTest_tpt(self: TypeTest) given Context: TypeTree

  def Pattern_TypeTest_module_apply(tpt: TypeTree) given Context: TypeTest
  def Pattern_TypeTest_module_copy(original: TypeTest)(tpt: TypeTree) given Context: TypeTest

  /** Pattern representing a `_` pattern */
  type WildcardPattern <: Pattern

  def matchPattern_WildcardPattern(pattern: Pattern) given Context: Option[WildcardPattern]

  def Pattern_WildcardPattern_module_apply(tpe: TypeOrBounds) given Context: WildcardPattern

  //
  // TYPES
  //

  /** Type or bounds */
  type TypeOrBounds <: AnyRef

  /** NoPrefix for a type selection */
  type NoPrefix <: TypeOrBounds

  def matchNoPrefix(x: TypeOrBounds) given Context: Option[NoPrefix]

  /** Type bounds */
  type TypeBounds <: TypeOrBounds

  def matchTypeBounds(x: TypeOrBounds) given Context: Option[TypeBounds]

  def TypeBounds_low(self: TypeBounds) given Context: Type
  def TypeBounds_hi(self: TypeBounds) given Context: Type

  /** A type */
  type Type <: TypeOrBounds

  def matchType(x: TypeOrBounds) given Context: Option[Type]

  def Type_apply(clazz: Class[_]) given Context: Type

  def `Type_=:=`(self: Type)(that: Type) given Context: Boolean
  def `Type_<:<`(self: Type)(that: Type) given Context: Boolean

  /** Widen from singleton type to its underlying non-singleton
    *  base type by applying one or more `underlying` dereferences,
    *  Also go from => T to T.
    *  Identity for all other types. Example:
    *
    *  class Outer { class C ; val x: C }
    *  def o: Outer
    *  <o.x.type>.widen = o.C
    */
  def Type_widen(self: Type) given Context: Type

  /** Follow aliases and dereferences LazyRefs, annotated types and instantiated
   *  TypeVars until type is no longer alias type, annotated type, LazyRef,
   *  or instantiated type variable.
   */
  def Type_dealias(self: Type) given Context: Type

  def Type_classSymbol(self: Type) given Context: Option[ClassDefSymbol]

  def Type_typeSymbol(self: Type) given Context: Symbol

  def Type_isSingleton(self: Type) given Context: Boolean

  def Type_memberType(self: Type)(member: Symbol) given Context: Type

  /** Is this type an instance of a non-bottom subclass of the given class `cls`? */
  def Type_derivesFrom(self: Type)(cls: ClassDefSymbol) given Context: Boolean

  /** Is this type a function type?
   *
   *  @return true if the dealised type of `self` without refinement is `FunctionN[T1, T2, ..., Tn]`
   *
   *  @note The function
   *
   *     - returns true for `given Int => Int` and `erased Int => Int`
   *     - returns false for `List[Int]`, despite that `List[Int] <:< Int => Int`.
   */
  def Type_isFunctionType(self: Type) given Context: Boolean


  /** Is this type an implicit function type?
   *
   *  @see `Type_isFunctionType`
   */
  def Type_isImplicitFunctionType(self: Type) given Context: Boolean

  /** Is this type an erased function type?
   *
   *  @see `Type_isFunctionType`
   */
  def Type_isErasedFunctionType(self: Type) given Context: Boolean

  /** Is this type a dependent function type?
   *
   *  @see `Type_isFunctionType`
   */
  def Type_isDependentFunctionType(self: Type) given Context: Boolean

  /** A singleton type representing a known constant value */
  type ConstantType <: Type

  def matchConstantType(tpe: TypeOrBounds) given Context: Option[ConstantType]

  def ConstantType_constant(self: ConstantType) given Context: Constant

  /** Type of a reference to a symbol */
  type SymRef <: Type

  def matchSymRef(tpe: TypeOrBounds) given Context: Option[SymRef]

  // TODO remove this method. May require splitting SymRef into TypeSymRef and TermSymRef
  def matchSymRef_unapply(tpe: TypeOrBounds) given Context: Option[(Symbol, TypeOrBounds /* Type | NoPrefix */)]

  def SymRef_qualifier(self: SymRef) given Context: TypeOrBounds

  /** Type of a reference to a term */
  type TermRef <: Type

  def matchTermRef(tpe: TypeOrBounds) given Context: Option[TermRef]

  def TermRef_name(self: TermRef) given Context: String
  def TermRef_qualifier(self: TermRef) given Context: TypeOrBounds

  def TermRef_apply(qual: TypeOrBounds, name: String) given Context: TermRef

  /** Type of a reference to a type */
  type TypeRef <: Type

  def matchTypeRef(tpe: TypeOrBounds) given Context: Option[TypeRef]

  def TypeRef_name(self: TypeRef) given Context: String
  def TypeRef_qualifier(self: TypeRef) given Context: TypeOrBounds

  /** Type of a `super` refernce */
  type SuperType <: Type

  def matchSuperType(tpe: TypeOrBounds) given Context: Option[SuperType]

  def SuperType_thistpe(self: SuperType) given Context: Type
  def SuperType_supertpe(self: SuperType) given Context: Type

  /** A type with a type refinement `T { type U }` */
  type Refinement <: Type

  def matchRefinement(tpe: TypeOrBounds) given Context: Option[Refinement]

  def Refinement_parent(self: Refinement) given Context: Type
  def Refinement_name(self: Refinement) given Context: String
  def Refinement_info(self: Refinement) given Context: TypeOrBounds

  /** A higher kinded type applied to some types `T[U]` */
  type AppliedType <: Type

  def matchAppliedType(tpe: TypeOrBounds) given Context: Option[AppliedType]

  def AppliedType_tycon(self: AppliedType) given Context: Type
  def AppliedType_args(self: AppliedType) given Context: List[TypeOrBounds]

  /** A type with an anottation `T @foo` */
  type AnnotatedType <: Type

  def matchAnnotatedType(tpe: TypeOrBounds) given Context: Option[AnnotatedType]

  def AnnotatedType_underlying(self: AnnotatedType) given Context: Type
  def AnnotatedType_annot(self: AnnotatedType) given Context: Term

  /** Intersection type `T & U` */
  type AndType <: Type

  def matchAndType(tpe: TypeOrBounds) given Context: Option[AndType]

  def AndType_left(self: AndType) given Context: Type
  def AndType_right(self: AndType) given Context: Type

  /** Union type `T | U` */
  type OrType <: Type

  def matchOrType(tpe: TypeOrBounds) given Context: Option[OrType]

  def OrType_left(self: OrType) given Context: Type
  def OrType_right(self: OrType) given Context: Type

  /** Type match `T match { case U => ... }` */
  type MatchType <: Type

  def matchMatchType(tpe: TypeOrBounds) given Context: Option[MatchType]

  def MatchType_bound(self: MatchType) given Context: Type
  def MatchType_scrutinee(self: MatchType) given Context: Type
  def MatchType_cases(self: MatchType) given Context: List[Type]

  /** Type of a by by name parameter */
  type ByNameType <: Type

  def matchByNameType(tpe: TypeOrBounds) given Context: Option[ByNameType]

  def ByNameType_underlying(self: ByNameType) given Context: Type

  /** Type of a parameter reference */
  type ParamRef <: Type

  def matchParamRef(tpe: TypeOrBounds) given Context: Option[ParamRef]

  def ParamRef_binder(self: ParamRef) given Context: LambdaType[TypeOrBounds]
  def ParamRef_paramNum(self: ParamRef) given Context: Int

  /** Type of `this` */
  type ThisType <: Type

  def matchThisType(tpe: TypeOrBounds) given Context: Option[ThisType]

  def ThisType_tref(self: ThisType) given Context: Type

  /** A type that is recursively defined `this` */
  type RecursiveThis <: Type

  def matchRecursiveThis(tpe: TypeOrBounds) given Context: Option[RecursiveThis]

  def RecursiveThis_binder(self: RecursiveThis) given Context: RecursiveType

  /** A type that is recursively defined */
  type RecursiveType <: Type

  def matchRecursiveType(tpe: TypeOrBounds) given Context: Option[RecursiveType]

  def RecursiveType_underlying(self: RecursiveType) given Context: Type

  // TODO can we add the bound back without an cake?
  // TODO is LambdaType really needed? ParamRefExtractor could be split into more precise extractors
  /** Common abstraction for lambda types (MethodType, PolyType and TypeLambda). */
  type LambdaType[ParamInfo /*<: TypeOrBounds*/] <: Type

  /** Type of the definition of a method taking a single list of parameters. It's return type may be a MethodType. */
  type MethodType <: LambdaType[Type]

  def matchMethodType(tpe: TypeOrBounds) given Context: Option[MethodType]

  def MethodType_isErased(self: MethodType): Boolean
  def MethodType_isImplicit(self: MethodType): Boolean
  def MethodType_paramNames(self: MethodType) given Context: List[String]
  def MethodType_paramTypes(self: MethodType) given Context: List[Type]
  def MethodType_resType(self: MethodType) given Context: Type

  /** Type of the definition of a method taking a list of type parameters. It's return type may be a MethodType. */
  type PolyType <: LambdaType[TypeBounds]

  def matchPolyType(tpe: TypeOrBounds) given Context: Option[PolyType]

  def PolyType_paramNames(self: PolyType) given Context: List[String]
  def PolyType_paramBounds(self: PolyType) given Context: List[TypeBounds]
  def PolyType_resType(self: PolyType) given Context: Type

  /** Type of the definition of a type lambda taking a list of type parameters. It's return type may be a TypeLambda. */
  type TypeLambda <: LambdaType[TypeBounds]

  def matchTypeLambda(tpe: TypeOrBounds) given Context: Option[TypeLambda]

  def TypeLambda_paramNames(self: TypeLambda) given Context: List[String]
  def TypeLambda_paramBounds(self: TypeLambda) given Context: List[TypeBounds]
  def TypeLambda_resType(self: TypeLambda) given Context: Type

  //
  // IMPORT SELECTORS
  //

  /** Import selectors:
   *  * SimpleSelector: `.bar` in `import foo.bar`
   *  * RenameSelector: `.{bar => baz}` in `import foo.{bar => baz}`
   *  * OmitSelector: `.{bar => _}` in `import foo.{bar => _}`
   */
  type ImportSelector <: AnyRef

  type SimpleSelector <: ImportSelector

  def matchSimpleSelector(self: ImportSelector) given Context: Option[SimpleSelector]

  def SimpleSelector_selection(self: SimpleSelector) given Context: Id

  type RenameSelector <: ImportSelector

  def matchRenameSelector(self: ImportSelector) given Context: Option[RenameSelector]

  def RenameSelector_from(self: RenameSelector) given Context: Id
  def RenameSelector_to(self: RenameSelector) given Context: Id

  type OmitSelector <: ImportSelector

  def matchOmitSelector(self: ImportSelector) given Context: Option[OmitSelector]

  def SimpleSelector_omited(self: OmitSelector) given Context: Id

  //
  // IDENTIFIERS
  //

  /** Untyped identifier */
  type Id <: AnyRef

  /** Position in the source code */
  def Id_pos(self: Id) given Context: Position

  /** Name of the identifier */
  def Id_name(self: Id) given Context: String

  //
  // SIGNATURES
  //

  /** JVM signature of a method */
  type Signature <: AnyRef

  /** The (JVM) erased signatures of the parameters */
  def Signature_paramSigs(self: Signature): List[String]

  /** The (JVM) erased result type */
  def Signature_resultSig(self: Signature): String

  //
  // POSITIONS
  //

  /** Position in a source file */
  type Position <: AnyRef

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

  //
  // SOURCE FILE
  //

  /** Scala source file */
  type SourceFile <: AnyRef

  /** Path to a source file */
  def SourceFile_jpath(self: SourceFile): java.nio.file.Path

  /** Content of a source file */
  def SourceFile_content(self: SourceFile): String

  //
  // COMMENTS
  //

  /** Comment */
  type Comment <: AnyRef

  def Comment_raw(self: Comment): String
  def Comment_expanded(self: Comment): Option[String]
  def Comment_usecases(self: Comment): List[(String, Option[DefDef])]

  //
  // CONSTANTS
  //

  /** Constant value represented as the constant itself */
  type Constant <: AnyRef

  def Constant_value(const: Constant): Any

  def matchConstant(constant: Constant): Option[Unit | Null | Int | Boolean | Byte | Short | Int | Long | Float | Double | Char | String | Type]
  def matchConstant_ClassTag(constant: Constant): Option[Type]

  def Constant_apply(x: Unit | Null | Int | Boolean | Byte | Short | Int | Long | Float | Double | Char | String | Type): Constant
  def Constant_ClassTag_apply(x: Type): Constant

  //
  // SYMBOLS
  //

  /** Symbol of a definition.
   *  Then can be compared with == to know if the definition is the same.
   */
  type Symbol <: AnyRef

  /** Owner of this symbol. The owner is the symbol in which this symbol is defined. */
  def Symbol_owner(self: Symbol) given Context: Symbol

  /** Flags of this symbol */
  def Symbol_flags(self: Symbol) given Context: Flags

  def Symbol_isLocalDummy(self: Symbol) given Context: Boolean

  def Symbol_isRefinementClass(self: Symbol) given Context: Boolean

  def Symbol_isAliasType(self: Symbol) given Context: Boolean

  def Symbol_isAnonymousClass(self: Symbol) given Context: Boolean

  def Symbol_isAnonymousFunction(self: Symbol) given Context: Boolean

  def Symbol_isAbstractType(self: Symbol) given Context: Boolean

  def Symbol_isClassConstructor(self: Symbol) given Context: Boolean

  def Symbol_isMethod(self: Symbol) given Context: Boolean

  /** This symbol is private within the resulting type. */
  def Symbol_privateWithin(self: Symbol) given Context: Option[Type]

  /** This symbol is protected within the resulting type. */
  def Symbol_protectedWithin(self: Symbol) given Context: Option[Type]

  /** The name of this symbol. */
  def Symbol_name(self: Symbol) given Context: String

  /** The full name of this symbol up to the root package. */
  def Symbol_fullName(self: Symbol) given Context: String

  /** The position of this symbol */
  def Symbol_pos(self: Symbol) given Context: Position

  def Symbol_localContext(self: Symbol) given Context: Context

  /** The comment of the symbol */
  def Symbol_comment(self: Symbol) given Context: Option[Comment]

  /** Annotations attached to this symbol */
  def Symbol_annots(self: Symbol) given Context: List[Term]

  def Symbol_isDefinedInCurrentRun(self: Symbol) given Context: Boolean

  /** Symbol of a package definition */
  type PackageDefSymbol <: Symbol

  def matchPackageDefSymbol(symbol: Symbol) given Context: Option[PackageDefSymbol]

  def PackageDefSymbol_tree(self: PackageDefSymbol) given Context: PackageDef

  type TypeSymbol <: Symbol

  def matchTypeSymbol(symbol: Symbol) given Context: Option[TypeSymbol]

  /** Symbol of a class definition. This includes anonymous class definitions and the class of a module object. */
  type ClassDefSymbol <: TypeSymbol

  def matchClassDefSymbol(symbol: Symbol) given Context: Option[ClassDefSymbol]

  /** ClassDef tree of this definition */
  def ClassDefSymbol_tree(self: ClassDefSymbol) given Context: ClassDef

  /** Fields directly declared in the class */
  def ClassDefSymbol_fields(self: Symbol) given Context: List[Symbol]

  /** Field with the given name directly declared in the class */
  def ClassDefSymbol_field(self: Symbol)(name: String) given Context: Option[Symbol]

  /** Get non-private named methods defined directly inside the class */
  def ClassDefSymbol_classMethod(self: Symbol)(name: String) given Context: List[DefDefSymbol]

  /** Get all non-private methods defined directly inside the class, excluding constructors */
  def ClassDefSymbol_classMethods(self: Symbol) given Context: List[DefDefSymbol]

  /** Get named non-private methods declared or inherited */
  def ClassDefSymbol_method(self: Symbol)(name: String) given Context: List[DefDefSymbol]

  /** Get all non-private methods declared or inherited */
  def ClassDefSymbol_methods(self: Symbol) given Context: List[DefDefSymbol]

  /** Fields of a case class type -- only the ones declared in primary constructor */
  def ClassDefSymbol_caseFields(self: Symbol) given Context: List[ValDefSymbol]

  /** The class symbol of the companion module class */
  def ClassDefSymbol_companionClass(self: Symbol) given Context: Option[ClassDefSymbol]

  /** The symbol of the companion module */
  def ClassDefSymbol_companionModule(self: Symbol) given Context: Option[ValDefSymbol]

  /** The symbol of the class of the companion module */
  def ClassDefSymbol_moduleClass(self: Symbol) given Context: Option[Symbol]

  def ClassDefSymbol_of(fullName: String) given Context: ClassDefSymbol

  /** Symbol of a type (parameter or member) definition. */
  type TypeDefSymbol <: TypeSymbol

  def matchTypeDefSymbol(symbol: Symbol) given Context: Option[TypeDefSymbol]

  def TypeDefSymbol_isTypeParam(self: TypeDefSymbol) given Context: Boolean

  /** TypeDef tree of this definition */
  def TypeDefSymbol_tree(self: TypeDefSymbol) given Context: TypeDef

  /** Symbol representing a bind definition. */
  type TypeBindSymbol <: TypeSymbol

  def matchTypeBindSymbol(symbol: Symbol) given Context: Option[TypeBindSymbol]

  /** TypeBind pattern of this definition */
  def TypeBindSymbol_tree(self: TypeBindSymbol) given Context: TypeBind

  type TermSymbol <: Symbol

  def matchTermSymbol(symbol: Symbol) given Context: Option[TermSymbol]

  /** Symbol representing a method definition. */
  type DefDefSymbol <: TermSymbol

  def matchDefDefSymbol(symbol: Symbol) given Context: Option[DefDefSymbol]

  /** DefDef tree of this definition */
  def DefDefSymbol_tree(self: DefDefSymbol) given Context: DefDef

  /** Signature of this definition */
  def DefDefSymbol_signature(self: DefDefSymbol) given Context: Signature

  /** Symbol representing a value definition. This includes `val`, `lazy val`, `var`, `object` and parameter definitions. */
  type ValDefSymbol <: TermSymbol

  def matchValDefSymbol(symbol: Symbol) given Context: Option[ValDefSymbol]

  /** ValDef tree of this definition */
  def ValDefSymbol_tree(self: ValDefSymbol) given Context: ValDef

  /** The class symbol of the companion module class */
  def ValDefSymbol_moduleClass(self: ValDefSymbol) given Context: Option[ClassDefSymbol]

  def ValDefSymbol_companionClass(self: ValDefSymbol) given Context: Option[ClassDefSymbol]

  /** Symbol representing a bind definition. */
  type BindSymbol <: TermSymbol

  def matchBindSymbol(symbol: Symbol) given Context: Option[BindSymbol]

  /** Bind pattern of this definition */
  def BindSymbol_tree(self: BindSymbol) given Context: Bind

  /** No symbol available. */
  type NoSymbol <: Symbol

  def matchNoSymbol(symbol: Symbol) given Context: Boolean

  //
  // FLAGS
  //

  /** FlagSet of a Symbol */
  type Flags

  /** Is the given flag set a subset of this flag sets */
  def Flags_is(self: Flags)(that: Flags): Boolean

  /** Union of the two flag sets */
  def Flags_or(self: Flags)(that: Flags): Flags

  /** Intersection of the two flag sets */
  def Flags_and(self: Flags)(that: Flags): Flags

  def Flags_EmptyFlags: Flags
  def Flags_Private: Flags
  def Flags_Protected: Flags
  def Flags_Abstract: Flags
  def Flags_Final: Flags
  def Flags_Sealed: Flags
  def Flags_Case: Flags
  def Flags_Implicit: Flags
  def Flags_Given: Flags
  def Flags_Erased: Flags
  def Flags_Lazy: Flags
  def Flags_Override: Flags
  def Flags_Inline: Flags
  def Flags_Macro: Flags
  def Flags_Static: Flags
  def Flags_JavaDefined: Flags
  def Flags_Object: Flags
  def Flags_Trait: Flags
  def Flags_Local: Flags
  def Flags_Synthetic: Flags
  def Flags_Artifact: Flags
  def Flags_Mutable: Flags
  def Flags_FieldAccessor: Flags
  def Flags_CaseAcessor: Flags
  def Flags_Covariant: Flags
  def Flags_Contravariant: Flags
  def Flags_Scala2X: Flags
  def Flags_DefaultParameterized: Flags
  def Flags_StableRealizable: Flags
  def Flags_Param: Flags
  def Flags_ParamAccessor: Flags
  def Flags_Enum: Flags
  def Flags_ModuleClass: Flags
  def Flags_PrivateLocal: Flags
  def Flags_Package: Flags

  //
  // QUOTED SEAL/UNSEAL
  //

  /** View this expression `quoted.Expr[_]` as a `Term` */
  def QuotedExpr_unseal(self: scala.quoted.Expr[_]) given Context: Term

  /** Checked cast to a `quoted.Expr[U]` */
  def QuotedExpr_cast[U](self: scala.quoted.Expr[_]) given (tp: scala.quoted.Type[U], ctx: Context): scala.quoted.Expr[U]

  /** View this expression `quoted.Type[T]` as a `TypeTree` */
  def QuotedType_unseal(self: scala.quoted.Type[_]) given Context: TypeTree

  /** Convert `Term` to an `quoted.Expr[Any]` */
  def QuotedExpr_seal(self: Term) given Context: scala.quoted.Expr[Any]


  /** Convert `Type` to an `quoted.Type[_]` */
  def QuotedType_seal(self: Type) given Context: scala.quoted.Type[_]

  //
  // DEFINITIONS
  //

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

  def Definitions_RepeatedParamClass: ClassDefSymbol

  def Definitions_OptionClass: Symbol
  def Definitions_NoneModule: Symbol
  def Definitions_SomeModule: Symbol

  def Definitions_ProductClass: Symbol
  // TODO avoid default parameters
  def Definitions_FunctionClass(arity: Int, isImplicit: Boolean = false, isErased: Boolean = false): Symbol

  def Definitions_TupleClass(arity: Int): Symbol

  /** Symbol of scala.internal.Quoted.patternHole */
  def Definitions_InternalQuoted_patternHole: Symbol

  /** Symbol of scala.internal.Quoted.patternBindHole */
  def Definitions_InternalQuoted_patternBindHoleAnnot: Symbol

  def Definitions_UnitType: Type
  def Definitions_ByteType: Type
  def Definitions_ShortType: Type
  def Definitions_CharType: Type
  def Definitions_IntType: Type
  def Definitions_LongType: Type
  def Definitions_FloatType: Type
  def Definitions_DoubleType: Type
  def Definitions_BooleanType: Type
  def Definitions_AnyType: Type
  def Definitions_AnyValType: Type
  def Definitions_AnyRefType: Type
  def Definitions_ObjectType: Type
  def Definitions_NothingType: Type
  def Definitions_NullType: Type
  def Definitions_StringType: Type

  //
  // IMPLICITS
  //

  type ImplicitSearchResult <: AnyRef

  type ImplicitSearchSuccess <: ImplicitSearchResult
  def matchImplicitSearchSuccess(isr: ImplicitSearchResult) given Context: Option[ImplicitSearchSuccess]
  def ImplicitSearchSuccess_tree(self: ImplicitSearchSuccess) given Context: Term

  type ImplicitSearchFailure <: ImplicitSearchResult
  def matchImplicitSearchFailure(isr: ImplicitSearchResult) given Context: Option[ImplicitSearchFailure]
  def ImplicitSearchFailure_explanation(self: ImplicitSearchFailure) given Context: String

  type DivergingImplicit <: ImplicitSearchFailure
  def matchDivergingImplicit(isr: ImplicitSearchResult) given Context: Option[DivergingImplicit]

  type NoMatchingImplicits <: ImplicitSearchFailure
  def matchNoMatchingImplicits(isr: ImplicitSearchResult) given Context: Option[NoMatchingImplicits]

  type AmbiguousImplicits <: ImplicitSearchFailure
  def matchAmbiguousImplicits(isr: ImplicitSearchResult) given Context: Option[AmbiguousImplicits]

  /** Find an implicit of type `T` in the current scope given by `ctx`.
   *  Return an `ImplicitSearchResult`.
   *
   *  @param tpe type of the implicit parameter
   *  @param ctx current context
   */
  def searchImplicit(tpe: Type) given Context: ImplicitSearchResult

}
