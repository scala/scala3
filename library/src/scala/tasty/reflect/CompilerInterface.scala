package scala.tasty.reflect // TODO move to scala.internal.tasty.reflect

import scala.quoted.QuoteContext
import scala.tasty.Reflection
import scala.internal.quoted.Unpickler

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
 *           |                             +- GivenMatch
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
*            |
 *           +- CaseDef
 *           +- TypeCaseDef
 *           |
 *           +- Bind
 *           +- Unapply
 *           +- Alternatives
 *
 *                   +- NoPrefix
 *  +- TypeOrBounds -+- TypeBounds
 *                   |
 *                   +- Type -------+- ConstantType
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
 *  +- Symbol
 *
 *  +- Flags
 *
 *  ```
 */
trait CompilerInterface {

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


  /////////////
  // CONTEXT //
  /////////////

  /** Compilation context */
  type Context <: AnyRef

  /** Returns the owner of the context */
  def Context_owner(self: Context): Symbol

  /** Returns the source file being compiled. The path is relative to the current working directory. */
  def Context_source(self: Context): java.nio.file.Path

  def Context_GADT_setFreshGADTBounds(self: Context): Context
  def Context_GADT_addToConstraint(self: Context)(syms: List[Symbol]): Boolean
  def Context_GADT_approximation(self: Context)(sym: Symbol, fromBelow: Boolean): Type

  /** Get package symbol if package is either defined in current compilation run or present on classpath. */
  def Context_requiredPackage(self: Context)(path: String): Symbol

  /** Get class symbol if class is either defined in current compilation run or present on classpath. */
  def Context_requiredClass(self: Context)(path: String): Symbol

  /** Get module symbol if module is either defined in current compilation run or present on classpath. */
  def Context_requiredModule(self: Context)(path: String): Symbol

  /** Get method symbol if method is either defined in current compilation run or present on classpath. Throws if the method has an overload. */
  def Context_requiredMethod(self: Context)(path: String): Symbol

  /** Returns true if we've tried to reflect on a Java class. */
  def Context_isJavaCompilationUnit(self: Context): Boolean

  /** Returns true if we've tried to reflect on a Scala2 (non-Tasty) class. */
  def Context_isScala2CompilationUnit(self: Context): Boolean

  /** Returns true if we've tried to reflect on a class that's already loaded (e.g. Option). */
  def Context_isAlreadyLoadedCompilationUnit(self: Context): Boolean

  /** Class name of the current CompilationUnit */
  def Context_compilationUnitClassname(self: Context): String


  ///////////////
  // REPORTING //
  ///////////////

  /** Report a compilation error with the given message at the given position */
  def error(msg: => String, pos: Position)(using ctx: Context): Unit

  /** Report a compilation error with the given message at the given position range */
  def error(msg: => String, source: SourceFile, start: Int, end: Int)(using ctx: Context): Unit

  /** Report a compilation warning with the given message at the given position */
  def warning(msg: => String, pos: Position)(using ctx: Context): Unit

  /** Report a compilation warning with the given message at the given position range */
  def warning(msg: => String, source: SourceFile, start: Int, end: Int)(using ctx: Context): Unit


  /////////////
  //  TREES  //
  /////////////

  /** Tree representing code written in the source */
  type Tree <: AnyRef

  def Tree_pos(self: Tree)(using ctx: Context): Position
  def Tree_symbol(self: Tree)(using ctx: Context): Symbol

  /** Tree representing a pacakage clause in the source code */
  type PackageClause <: Tree

  def PackageClause_TypeTest(using ctx: Context): TypeTest[Tree, PackageClause]

  def PackageClause_pid(self: PackageClause)(using ctx: Context): Ref
  def PackageClause_stats(self: PackageClause)(using ctx: Context): List[Tree]

  def PackageClause_apply(pid: Ref, stats: List[Tree])(using ctx: Context): PackageClause

  def PackageClause_copy(original: Tree)(pid: Ref, stats: List[Tree])(using ctx: Context): PackageClause

  /** Tree representing a statement in the source code */
  type Statement <: Tree

  def Statement_TypeTest(using ctx: Context): TypeTest[Tree, Statement]

  /** Tree representing an import in the source code */
  type Import <: Statement

  def Import_TypeTest(using ctx: Context): TypeTest[Tree, Import]

  def Import_implied(self: Import): Boolean
  def Import_expr(self: Import)(using ctx: Context): Term
  def Import_selectors(self: Import)(using ctx: Context): List[ImportSelector]

  def Import_apply(iexpr: Term, selectors: List[ImportSelector])(using ctx: Context): Import

  def Import_copy(original: Tree)(expr: Term, selectors: List[ImportSelector])(using ctx: Context): Import

  /** Tree representing a definition in the source code. It can be `PackageDef`, `ClassDef`, `TypeDef`, `DefDef` or `ValDef` */
  type Definition <: Statement

  def Definition_TypeTest(using ctx: Context): TypeTest[Tree, Definition]

  def Definition_name(self: Definition)(using ctx: Context): String

  /** Tree representing a package definition. This includes definitions in all source files */
  type PackageDef <: Definition

  def PackageDef_TypeTest(using ctx: Context): TypeTest[Tree, PackageDef]

  def PackageDef_owner(self: PackageDef)(using ctx: Context): PackageDef
  def PackageDef_members(self: PackageDef)(using ctx: Context): List[Statement]

  /** Tree representing a class definition. This includes annonymus class definitions and the class of a module object */
  type ClassDef <: Definition

  def ClassDef_TypeTest(using ctx: Context): TypeTest[Tree, ClassDef]

  def ClassDef_constructor(self: ClassDef)(using ctx: Context): DefDef
  def ClassDef_parents(self: ClassDef)(using ctx: Context): List[Tree/* Term | TypeTree */]
  def ClassDef_derived(self: ClassDef)(using ctx: Context): List[TypeTree]
  def ClassDef_self(self: ClassDef)(using ctx: Context): Option[ValDef]
  def ClassDef_body(self: ClassDef)(using ctx: Context): List[Statement]

  def ClassDef_copy(original: Tree)(name: String, constr: DefDef, parents: List[Tree/* Term | TypeTree */], derived: List[TypeTree], selfOpt: Option[ValDef], body: List[Statement])(using ctx: Context): ClassDef

  /** Tree representing a type (parameter or member) definition in the source code */
  type TypeDef <: Definition

  def TypeDef_TypeTest(using ctx: Context): TypeTest[Tree, TypeDef]

  def TypeDef_rhs(self: TypeDef)(using ctx: Context): Tree /*TypeTree | TypeBoundsTree*/

  def TypeDef_apply(symbol: Symbol)(using ctx: Context): TypeDef
  def TypeDef_copy(original: Tree)(name: String, rhs: Tree /*TypeTree | TypeBoundsTree*/)(using ctx: Context): TypeDef

  /** Tree representing a method definition in the source code */
  type DefDef <: Definition

  def DefDef_TypeTest(using ctx: Context): TypeTest[Tree, DefDef]

  def DefDef_typeParams(self: DefDef)(using ctx: Context): List[TypeDef]
  def DefDef_paramss(self: DefDef)(using ctx: Context): List[List[ValDef]]
  def DefDef_returnTpt(self: DefDef)(using ctx: Context): TypeTree
  def DefDef_rhs(self: DefDef)(using ctx: Context): Option[Term]

  def DefDef_apply(symbol: Symbol, rhsFn: List[Type] => List[List[Term]] => Option[Term])(using ctx: Context): DefDef
  def DefDef_copy(original: Tree)(name: String, typeParams: List[TypeDef], paramss: List[List[ValDef]], tpt: TypeTree, rhs: Option[Term])(using ctx: Context): DefDef

  /** Tree representing a value definition in the source code This inclues `val`, `lazy val`, `var`, `object` and parameter definitions. */
  type ValDef <: Definition

  def ValDef_TypeTest(using ctx: Context): TypeTest[Tree, ValDef]

  def ValDef_tpt(self: ValDef)(using ctx: Context): TypeTree
  def ValDef_rhs(self: ValDef)(using ctx: Context): Option[Term]

  def ValDef_apply(symbol: Symbol, rhs: Option[Term])(using ctx: Context): ValDef
  def ValDef_copy(original: Tree)(name: String, tpt: TypeTree, rhs: Option[Term])(using ctx: Context): ValDef

  /** Tree representing an expression in the source code */
  type Term <: Statement

  def Term_TypeTest(using ctx: Context): TypeTest[Tree, Term]

  def Term_tpe(self: Term)(using ctx: Context): Type
  def Term_underlyingArgument(self: Term)(using ctx: Context): Term
  def Term_underlying(self: Term)(using ctx: Context): Term
  def Term_etaExpand(term: Term)(using ctx: Context): Term

  /** Tree representing a reference to definition */
  type Ref <: Term

  def Ref_TypeTest(using ctx: Context): TypeTest[Tree, Ref]

  /** A tree representing the same reference as the given type */
  def Ref_term(tp: TermRef)(using ctx: Context): Ref

  def Ref_apply(sym: Symbol)(using ctx: Context): Ref

  /** Tree representing a reference to definition with a given name */
  type Ident <: Ref

  def Ident_TypeTest(using ctx: Context): TypeTest[Tree, Ident]

  def Ident_name(self: Ident)(using ctx: Context): String

  def Ident_apply(tmref: TermRef)(using ctx: Context): Term
  def Ident_copy(original: Tree)(name: String)(using ctx: Context): Ident

  /** Tree representing a selection of definition with a given name on a given prefix */
  type Select <: Ref

  def Select_TypeTest(using ctx: Context): TypeTest[Tree, Select]

  def Select_qualifier(self: Select)(using ctx: Context): Term
  def Select_name(self: Select)(using ctx: Context): String
  def Select_signature(self: Select)(using ctx: Context): Option[Signature]

  def Select_apply(qualifier: Term, symbol: Symbol)(using ctx: Context): Select
  def Select_unique(qualifier: Term, name: String)(using ctx: Context): Select
  // TODO rename, this returns an Apply and not a Select
  def Select_overloaded(qualifier: Term, name: String, targs: List[Type], args: List[Term])(using ctx: Context): Apply
  def Select_copy(original: Tree)(qualifier: Term, name: String)(using ctx: Context): Select

  /** Tree representing a literal value in the source code */
  type Literal <: Term

  def Literal_TypeTest(using ctx: Context): TypeTest[Tree, Literal]

  def Literal_constant(self: Literal)(using ctx: Context): Constant

  def Literal_apply(constant: Constant)(using ctx: Context): Literal
  def Literal_copy(original: Tree)(constant: Constant)(using ctx: Context): Literal

  /** Tree representing `this` in the source code */
  type This <: Term

  def This_TypeTest(using ctx: Context): TypeTest[Tree, This]

  def This_id(self: This)(using ctx: Context): Option[Id]

  def This_apply(cls: Symbol)(using ctx: Context): This
  def This_copy(original: Tree)(qual: Option[Id])(using ctx: Context): This

  /** Tree representing `new` in the source code */
  type New <: Term

  def New_TypeTest(using ctx: Context): TypeTest[Tree, New]

  def New_tpt(self: New)(using ctx: Context): TypeTree

  def New_apply(tpt: TypeTree)(using ctx: Context): New
  def New_copy(original: Tree)(tpt: TypeTree)(using ctx: Context): New

  /** Tree representing an argument passed with an explicit name. Such as `arg1 = x` in `foo(arg1 = x)` */
  type NamedArg <: Term

  def NamedArg_TypeTest(using ctx: Context): TypeTest[Tree, NamedArg]

  def NamedArg_name(self: NamedArg)(using ctx: Context): String
  def NamedArg_value(self: NamedArg)(using ctx: Context): Term

  def NamedArg_apply(name: String, arg: Term)(using ctx: Context): NamedArg
  def NamedArg_copy(original: Tree)(name: String, arg: Term)(using ctx: Context): NamedArg

  /** Tree an application of arguments. It represents a single list of arguments, multiple argument lists will have nested `Apply`s */
  type Apply <: Term

  def Apply_TypeTest(using ctx: Context): TypeTest[Tree, Apply]

  def Apply_fun(self: Apply)(using ctx: Context): Term
  def Apply_args(self: Apply)(using ctx: Context): List[Term]

  def Apply_apply(fn: Term, args: List[Term])(using ctx: Context): Apply
  def Apply_copy(original: Tree)(fun: Term, args: List[Term])(using ctx: Context): Apply

  /** Tree an application of type arguments */
  type TypeApply <: Term

  def TypeApply_TypeTest(using ctx: Context): TypeTest[Tree, TypeApply]

  def TypeApply_fun(self: TypeApply)(using ctx: Context): Term
  def TypeApply_args(self: TypeApply)(using ctx: Context): List[TypeTree]

  def TypeApply_apply(fn: Term, args: List[TypeTree])(using ctx: Context): TypeApply
  def TypeApply_copy(original: Tree)(fun: Term, args: List[TypeTree])(using ctx: Context): TypeApply

  /** Tree representing `super` in the source code */
  type Super <: Term

  def Super_TypeTest(using ctx: Context): TypeTest[Tree, Super]

  def Super_qualifier(self: Super)(using ctx: Context): Term
  def Super_id(self: Super)(using ctx: Context): Option[Id]

  def Super_apply(qual: Term, mix: Option[Id])(using ctx: Context): Super
  def Super_copy(original: Tree)(qual: Term, mix: Option[Id])(using ctx: Context): Super

  /** Tree representing a type ascription `x: T` in the source code */
  type Typed <: Term

  def Typed_TypeTest(using ctx: Context): TypeTest[Tree, Typed]

  def Typed_expr(self: Typed)(using ctx: Context): Term
  def Typed_tpt(self: Typed)(using ctx: Context): TypeTree

  def Typed_apply(expr: Term, tpt: TypeTree)(using ctx: Context): Typed
  def Typed_copy(original: Tree)(expr: Term, tpt: TypeTree)(using ctx: Context): Typed

  /** Tree representing an assignment `x = y` in the source code */
  type Assign <: Term

  def Assign_TypeTest(using ctx: Context): TypeTest[Tree, Assign]

  def Assign_lhs(self: Assign)(using ctx: Context): Term
  def Assign_rhs(self: Assign)(using ctx: Context): Term

  def Assign_apply(lhs: Term, rhs: Term)(using ctx: Context): Assign
  def Assign_copy(original: Tree)(lhs: Term, rhs: Term)(using ctx: Context): Assign

  /** Tree representing a block `{ ... }` in the source code */
  type Block <: Term

  def Block_TypeTest(using ctx: Context): TypeTest[Tree, Block]

  def Block_statements(self: Block)(using ctx: Context): List[Statement]
  def Block_expr(self: Block)(using ctx: Context): Term

  def Block_apply(stats: List[Statement], expr: Term)(using ctx: Context): Block
  def Block_copy(original: Tree)(stats: List[Statement], expr: Term)(using ctx: Context): Block

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

  def Closure_TypeTest(using ctx: Context): TypeTest[Tree, Closure]

  def Closure_meth(self: Closure)(using ctx: Context): Term
  def Closure_tpeOpt(self: Closure)(using ctx: Context): Option[Type]

  def Closure_apply(meth: Term, tpe: Option[Type])(using ctx: Context): Closure
  def Closure_copy(original: Tree)(meth: Tree, tpe: Option[Type])(using ctx: Context): Closure

  def Lambda_apply(tpe: MethodType, rhsFn: List[Tree] => Tree)(using ctx: Context): Block

  /** Tree representing an if/then/else `if (...) ... else ...` in the source code */
  type If <: Term

  def If_TypeTest(using ctx: Context): TypeTest[Tree, If]

  def If_cond(self: If)(using ctx: Context): Term
  def If_thenp(self: If)(using ctx: Context): Term
  def If_elsep(self: If)(using ctx: Context): Term

  def If_apply(cond: Term, thenp: Term, elsep: Term)(using ctx: Context): If
  def If_copy(original: Tree)(cond: Term, thenp: Term, elsep: Term)(using ctx: Context): If

  /** Tree representing a pattern match `x match  { ... }` in the source code */
  type Match <: Term

  def Match_TypeTest(using ctx: Context): TypeTest[Tree, Match]

  def Match_scrutinee(self: Match)(using ctx: Context): Term
  def Match_cases(self: Match)(using ctx: Context): List[CaseDef]

  def Match_apply(selector: Term, cases: List[CaseDef])(using ctx: Context): Match
  def Match_copy(original: Tree)(selector: Term, cases: List[CaseDef])(using ctx: Context): Match

  /** Tree representing a pattern match `given match  { ... }` in the source code */
  type GivenMatch <: Term

  def GivenMatch_TypeTest(using ctx: Context): TypeTest[Tree, GivenMatch]

  def GivenMatch_cases(self: GivenMatch)(using ctx: Context): List[CaseDef]

  def GivenMatch_apply(cases: List[CaseDef])(using ctx: Context): GivenMatch
  def GivenMatch_copy(original: Tree)(cases: List[CaseDef])(using ctx: Context): GivenMatch

  /** Tree representing a tyr catch `try x catch { ... } finally { ... }` in the source code */
  type Try <: Term

  def Try_TypeTest(using ctx: Context): TypeTest[Tree, Try]

  def Try_body(self: Try)(using ctx: Context): Term
  def Try_cases(self: Try)(using ctx: Context): List[CaseDef]
  def Try_finalizer(self: Try)(using ctx: Context): Option[Term]

  def Try_apply(expr: Term, cases: List[CaseDef], finalizer: Option[Term])(using ctx: Context): Try
  def Try_copy(original: Tree)(expr: Term, cases: List[CaseDef], finalizer: Option[Term])(using ctx: Context): Try

  /** Tree representing a `return` in the source code */
  type Return <: Term

  def Return_TypeTest(using ctx: Context): TypeTest[Tree, Return]

  def Return_expr(self: Return)(using ctx: Context): Term

  def Return_apply(expr: Term)(using ctx: Context): Return
  def Return_copy(original: Tree)(expr: Term)(using ctx: Context): Return

  /** Tree representing a variable argument list in the source code */
  type Repeated <: Term

  def Repeated_TypeTest(using ctx: Context): TypeTest[Tree, Repeated]

  def Repeated_elems(self: Repeated)(using ctx: Context): List[Term]
  def Repeated_elemtpt(self: Repeated)(using ctx: Context): TypeTree

  def Repeated_apply(elems: List[Term], elemtpt: TypeTree)(using ctx: Context): Repeated
  def Repeated_copy(original: Tree)(elems: List[Term], elemtpt: TypeTree)(using ctx: Context): Repeated

  /** Tree representing the scope of an inlined tree */
  type Inlined <: Term

  def Inlined_TypeTest(using ctx: Context): TypeTest[Tree, Inlined]

  def Inlined_call(self: Inlined)(using ctx: Context): Option[Tree/* Term | TypeTree */]
  def Inlined_bindings(self: Inlined)(using ctx: Context): List[Definition]
  def Inlined_body(self: Inlined)(using ctx: Context): Term

  def Inlined_apply(call: Option[Tree/* Term | TypeTree */], bindings: List[Definition], expansion: Term)(using ctx: Context): Inlined
  def Inlined_copy(original: Tree)(call: Option[Tree/* Term | TypeTree */], bindings: List[Definition], expansion: Term)(using ctx: Context): Inlined

  /** Tree representing a selection of definition with a given name on a given prefix and number of nested scopes of inlined trees */
  type SelectOuter <: Term

  def SelectOuter_TypeTest(using ctx: Context): TypeTest[Tree, SelectOuter]

  def SelectOuter_qualifier(self: SelectOuter)(using ctx: Context): Term
  def SelectOuter_level(self: SelectOuter)(using ctx: Context): Int

  def SelectOuter_apply(qualifier: Term, name: String, levels: Int)(using ctx: Context): SelectOuter
  def SelectOuter_copy(original: Tree)(qualifier: Term, name: String, levels: Int)(using ctx: Context): SelectOuter

  /** Tree representing a while loop */
  type While <: Term

  def While_TypeTest(using ctx: Context): TypeTest[Tree, While]

  def While_cond(self: While)(using ctx: Context): Term
  def While_body(self: While)(using ctx: Context): Term

  def While_apply(cond: Term, body: Term)(using ctx: Context): While
  def While_copy(original: Tree)(cond: Term, body: Term)(using ctx: Context): While

  /** Type tree representing a type written in the source */
  type TypeTree <: Tree

  def TypeTree_TypeTest(using ctx: Context): TypeTest[Tree, TypeTree]

  def TypeTree_tpe(self: TypeTree)(using ctx: Context): Type

  /** Type tree representing an inferred type */
  type Inferred <: TypeTree

  def Inferred_TypeTest(using ctx: Context): TypeTest[Tree, Inferred]

  def Inferred_apply(tpe: Type)(using ctx: Context): Inferred

  def TypeRef_apply(sym: Symbol)(using ctx: Context): TypeTree

  /** Type tree representing a reference to definition with a given name */
  type TypeIdent <: TypeTree

  def TypeIdent_TypeTest(using ctx: Context): TypeTest[Tree, TypeIdent]

  def TypeIdent_name(self: TypeIdent)(using ctx: Context): String

  def TypeIdent_copy(original: Tree)(name: String)(using ctx: Context): TypeIdent

  /** Type tree representing a selection of definition with a given name on a given term prefix */
  type TypeSelect <: TypeTree

  def TypeSelect_TypeTest(using ctx: Context): TypeTest[Tree, TypeSelect]

  def TypeSelect_qualifier(self: TypeSelect)(using ctx: Context): Term
  def TypeSelect_name(self: TypeSelect)(using ctx: Context): String

  def TypeSelect_apply(qualifier: Term, name: String)(using ctx: Context): TypeSelect
  def TypeSelect_copy(original: Tree)(qualifier: Term, name: String)(using ctx: Context): TypeSelect

  /** Type tree representing a selection of definition with a given name on a given type prefix */
  type Projection <: TypeTree

  def Projection_TypeTest(using ctx: Context): TypeTest[Tree, Projection]

  def Projection_qualifier(self: Projection)(using ctx: Context): TypeTree
  def Projection_name(self: Projection)(using ctx: Context): String

  def Projection_copy(original: Tree)(qualifier: TypeTree, name: String)(using ctx: Context): Projection

  /** Type tree representing a singleton type */
  type Singleton <: TypeTree

  def Singleton_TypeTest(using ctx: Context): TypeTest[Tree, Singleton]

  def Singleton_ref(self: Singleton)(using ctx: Context): Term

  def Singleton_apply(ref: Term)(using ctx: Context): Singleton
  def Singleton_copy(original: Tree)(ref: Term)(using ctx: Context): Singleton

  /** Type tree representing a type refinement */
  type Refined <: TypeTree

  def Refined_TypeTest(using ctx: Context): TypeTest[Tree, Refined]

  def Refined_tpt(self: Refined)(using ctx: Context): TypeTree
  def Refined_refinements(self: Refined)(using ctx: Context): List[Definition]

  def Refined_copy(original: Tree)(tpt: TypeTree, refinements: List[Definition])(using ctx: Context): Refined

  /** Type tree representing a type application */
  type Applied <: TypeTree

  def Applied_TypeTest(using ctx: Context): TypeTest[Tree, Applied]

  def Applied_tpt(self: Applied)(using ctx: Context): TypeTree
  def Applied_args(self: Applied)(using ctx: Context): List[Tree /*TypeTree | TypeBoundsTree*/]

  def Applied_apply(tpt: TypeTree, args: List[Tree /*TypeTree | TypeBoundsTree*/])(using ctx: Context): Applied
  def Applied_copy(original: Tree)(tpt: TypeTree, args: List[Tree /*TypeTree | TypeBoundsTree*/])(using ctx: Context): Applied

  /** Type tree representing an annotated type */
  type Annotated <: TypeTree

  def Annotated_TypeTest(using ctx: Context): TypeTest[Tree, Annotated]

  def Annotated_arg(self: Annotated)(using ctx: Context): TypeTree
  def Annotated_annotation(self: Annotated)(using ctx: Context): Term

  def Annotated_apply(arg: TypeTree, annotation: Term)(using ctx: Context): Annotated
  def Annotated_copy(original: Tree)(arg: TypeTree, annotation: Term)(using ctx: Context): Annotated

  /** Type tree representing a type match */
  type MatchTypeTree <: TypeTree

  def MatchTypeTree_TypeTest(using ctx: Context): TypeTest[Tree, MatchTypeTree]

  def MatchTypeTree_bound(self: MatchTypeTree)(using ctx: Context): Option[TypeTree]
  def MatchTypeTree_selector(self: MatchTypeTree)(using ctx: Context): TypeTree
  def MatchTypeTree_cases(self: MatchTypeTree)(using ctx: Context): List[TypeCaseDef]

  def MatchTypeTree_apply(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef])(using ctx: Context): MatchTypeTree
  def MatchTypeTree_copy(original: Tree)(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef])(using ctx: Context): MatchTypeTree

  /** Type tree representing a by name parameter */
  type ByName <: TypeTree

  def ByName_result(self: ByName)(using ctx: Context): TypeTree

  def ByName_TypeTest(using ctx: Context): TypeTest[Tree, ByName]

  def ByName_apply(result: TypeTree)(using ctx: Context): ByName
  def ByName_copy(original: Tree)(result: TypeTree)(using ctx: Context): ByName

  /** Type tree representing a lambda abstraction type */
  type LambdaTypeTree <: TypeTree

  def LambdaTypeTree_TypeTest(using ctx: Context): TypeTest[Tree, LambdaTypeTree]

  def Lambdatparams(self: LambdaTypeTree)(using ctx: Context): List[TypeDef]
  def Lambdabody(self: LambdaTypeTree)(using ctx: Context): Tree /*TypeTree | TypeBoundsTree*/

  def Lambdaapply(tparams: List[TypeDef], body: Tree /*TypeTree | TypeBoundsTree*/)(using ctx: Context): LambdaTypeTree
  def Lambdacopy(original: Tree)(tparams: List[TypeDef], body: Tree /*TypeTree | TypeBoundsTree*/)(using ctx: Context): LambdaTypeTree

  /** Type tree representing a type binding */
  type TypeBind <: TypeTree

  def TypeBind_TypeTest(using ctx: Context): TypeTest[Tree, TypeBind]

  def TypeBind_name(self: TypeBind)(using ctx: Context): String
  def TypeBind_body(self: TypeBind)(using ctx: Context): Tree /*TypeTree | TypeBoundsTree*/

  def TypeBind_copy(original: Tree)(name: String, tpt: Tree /*TypeTree | TypeBoundsTree*/)(using ctx: Context): TypeBind

  /** Type tree within a block with aliases `{ type U1 = ... ; T[U1, U2] }` */
  type TypeBlock <: TypeTree

  def TypeBlock_TypeTest(using ctx: Context): TypeTest[Tree, TypeBlock]

  def TypeBlock_aliases(self: TypeBlock)(using ctx: Context): List[TypeDef]
  def TypeBlock_tpt(self: TypeBlock)(using ctx: Context): TypeTree

  def TypeBlock_apply(aliases: List[TypeDef], tpt: TypeTree)(using ctx: Context): TypeBlock
  def TypeBlock_copy(original: Tree)(aliases: List[TypeDef], tpt: TypeTree)(using ctx: Context): TypeBlock

  /** Type tree representing a type bound written in the source */
  type TypeBoundsTree <: Tree /*TypeTree | TypeBoundsTree*/

  def TypeBoundsTree_TypeTest(using ctx: Context): TypeTest[Tree, TypeBoundsTree]

  def TypeBoundsTree_tpe(self: TypeBoundsTree)(using ctx: Context): TypeBounds
  def TypeBoundsTree_low(self: TypeBoundsTree)(using ctx: Context): TypeTree
  def TypeBoundsTree_hi(self: TypeBoundsTree)(using ctx: Context): TypeTree

  /** Type tree representing wildcard type bounds written in the source.
    *  The wildcard type `_` (for example in in `List[_]`) will be a type tree that
    *  represents a type but has `TypeBound`a inside.
    */
  type WildcardTypeTree <: Tree

  def WildcardTypeTree_TypeTest(using ctx: Context): TypeTest[Tree, WildcardTypeTree]

  def WildcardTypeTree_tpe(self: WildcardTypeTree)(using ctx: Context): TypeOrBounds

  /** Branch of a pattern match or catch clause */
  type CaseDef <: Tree

  def CaseDef_TypeTest(using ctx: Context): TypeTest[Tree, CaseDef]

  def CaseDef_pattern(self: CaseDef)(using ctx: Context): Tree
  def CaseDef_guard(self: CaseDef)(using ctx: Context): Option[Term]
  def CaseDef_rhs(self: CaseDef)(using ctx: Context): Term

  def CaseDef_module_apply(pattern: Tree, guard: Option[Term], body: Term)(using ctx: Context): CaseDef
  def CaseDef_module_copy(original: Tree)(pattern: Tree, guard: Option[Term], body: Term)(using ctx: Context): CaseDef

  /** Branch of a type pattern match */
  type TypeCaseDef <: Tree

  def TypeCaseDef_TypeTest(using ctx: Context): TypeTest[Tree, TypeCaseDef]

  def TypeCaseDef_pattern(self: TypeCaseDef)(using ctx: Context): TypeTree
  def TypeCaseDef_rhs(self: TypeCaseDef)(using ctx: Context): TypeTree

  def TypeCaseDef_module_apply(pattern: TypeTree, body: TypeTree)(using ctx: Context): TypeCaseDef
  def TypeCaseDef_module_copy(original: Tree)(pattern: TypeTree, body: TypeTree)(using ctx: Context): TypeCaseDef

  //
  // PATTERNS
  //

  /** Tree representing a binding pattern `_ @ _` */
  type Bind <: Tree

  def Bind_TypeTest(using ctx: Context): TypeTest[Tree, Bind]

  def Tree_Bind_name(self: Bind)(using ctx: Context): String

  def Tree_Bind_pattern(self: Bind)(using ctx: Context): Tree

  def Tree_Bind_module_apply(sym: Symbol, body: Tree)(using ctx: Context): Bind

  def Tree_Bind_module_copy(original: Tree)(name: String, pattern: Tree)(using ctx: Context): Bind

  /** Tree representing an unapply pattern `Xyz(...)` */
  type Unapply <: Tree

  def Unapply_TypeTest(using ctx: Context): TypeTest[Tree, Unapply]

  def Tree_Unapply_fun(self: Unapply)(using ctx: Context): Term

  def Tree_Unapply_implicits(self: Unapply)(using ctx: Context): List[Term]

  def Tree_Unapply_patterns(self: Unapply)(using ctx: Context): List[Tree]

  def Tree_Unapply_module_copy(original: Tree)(fun: Term, implicits: List[Term], patterns: List[Tree])(using ctx: Context): Unapply

  /** Tree representing pattern alternatives `X | Y | ...` */
  type Alternatives <: Tree

  def Alternatives_TypeTest(using ctx: Context): TypeTest[Tree, Alternatives]

  def Tree_Alternatives_patterns(self: Alternatives)(using ctx: Context): List[Tree]

  def Tree_Alternatives_module_apply(patterns: List[Tree])(using ctx: Context): Alternatives
  def Tree_Alternatives_module_copy(original: Tree)(patterns: List[Tree])(using ctx: Context): Alternatives


  //
  // TYPES
  //

  /** Type or bounds */
  type TypeOrBounds <: AnyRef

  /** NoPrefix for a type selection */
  type NoPrefix <: TypeOrBounds

  def NoPrefix_TypeTest(using ctx: Context): TypeTest[Tree, NoPrefix]

  /** Type bounds */
  type TypeBounds <: TypeOrBounds

  def TypeBounds_TypeTest(using ctx: Context): TypeTest[TypeOrBounds, TypeBounds]

  def TypeBounds_apply(low: Type, hi: Type)(using ctx: Context): TypeBounds

  def TypeBounds_low(self: TypeBounds)(using ctx: Context): Type
  def TypeBounds_hi(self: TypeBounds)(using ctx: Context): Type

  /** A type */
  type Type <: TypeOrBounds

  def Type_TypeTest(using ctx: Context): TypeTest[TypeOrBounds, Type]

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

  /** A singleton type representing a known constant value */
  type ConstantType <: Type

  def ConstantType_TypeTest(using ctx: Context): TypeTest[TypeOrBounds, ConstantType]

  def ConstantType_apply(const : Constant)(using ctx : Context) : ConstantType

  def ConstantType_constant(self: ConstantType)(using ctx: Context): Constant

  /** Type of a reference to a term symbol */
  type TermRef <: Type

  def TermRef_TypeTest(using ctx: Context): TypeTest[TypeOrBounds, TermRef]

  def TermRef_apply(qual: TypeOrBounds, name: String)(using ctx: Context): TermRef

  def TermRef_qualifier(self: TermRef)(using ctx: Context): TypeOrBounds
  def TermRef_name(self: TermRef)(using ctx: Context): String

  /** Type of a reference to a type symbol */
  type TypeRef <: Type

  def TypeRef_TypeTest(using ctx: Context): TypeTest[TypeOrBounds, TypeRef]

  def TypeRef_qualifier(self: TypeRef)(using ctx: Context): TypeOrBounds
  def TypeRef_name(self: TypeRef)(using ctx: Context): String
  def TypeRef_isOpaqueAlias(self: TypeRef)(using ctx: Context): Boolean
  def TypeRef_translucentSuperType(self: TypeRef)(using ctx: Context): Type

  /** Type of a `super` reference */
  type SuperType <: Type

  def SuperType_TypeTest(using ctx: Context): TypeTest[TypeOrBounds, SuperType]

  def SuperType_apply(thistpe: Type, supertpe: Type)(using ctx: Context): SuperType

  def SuperType_thistpe(self: SuperType)(using ctx: Context): Type
  def SuperType_supertpe(self: SuperType)(using ctx: Context): Type

  /** A type with a type refinement `T { type U }` */
  type Refinement <: Type

  def Refinement_TypeTest(using ctx: Context): TypeTest[TypeOrBounds, Refinement]

  def Refinement_apply(parent: Type, name: String, info: TypeOrBounds /* Type | TypeBounds */)(using ctx: Context): Refinement

  def Refinement_parent(self: Refinement)(using ctx: Context): Type
  def Refinement_name(self: Refinement)(using ctx: Context): String
  def Refinement_info(self: Refinement)(using ctx: Context): TypeOrBounds

  /** A higher kinded type applied to some types `T[U]` */
  type AppliedType <: Type

  def AppliedType_TypeTest(using ctx: Context): TypeTest[TypeOrBounds, AppliedType]

  def AppliedType_tycon(self: AppliedType)(using ctx: Context): Type
  def AppliedType_args(self: AppliedType)(using ctx: Context): List[TypeOrBounds]

  def AppliedType_apply(tycon: Type, args: List[TypeOrBounds])(using ctx: Context) : AppliedType

  /** A type with an anottation `T @foo` */
  type AnnotatedType <: Type

  def AnnotatedType_TypeTest(using ctx: Context): TypeTest[TypeOrBounds, AnnotatedType]

  def AnnotatedType_apply(underlying: Type, annot: Term)(using ctx: Context): AnnotatedType

  def AnnotatedType_underlying(self: AnnotatedType)(using ctx: Context): Type
  def AnnotatedType_annot(self: AnnotatedType)(using ctx: Context): Term

  /** Intersection type `T & U` */
  type AndType <: Type

  def AndType_TypeTest(using ctx: Context): TypeTest[TypeOrBounds, AndType]

  def AndType_apply(lhs: Type, rhs: Type)(using ctx: Context): AndType

  def AndType_left(self: AndType)(using ctx: Context): Type
  def AndType_right(self: AndType)(using ctx: Context): Type

  /** Union type `T | U` */
  type OrType <: Type

  def OrType_TypeTest(using ctx: Context): TypeTest[TypeOrBounds, OrType]

  def OrType_apply(lhs : Type, rhs : Type)(using ctx : Context): OrType

  def OrType_left(self: OrType)(using ctx: Context): Type
  def OrType_right(self: OrType)(using ctx: Context): Type

  /** Type match `T match { case U => ... }` */
  type MatchType <: Type

  def MatchType_TypeTest(using ctx: Context): TypeTest[TypeOrBounds, MatchType]

  def MatchType_apply(bound: Type, scrutinee: Type, cases: List[Type])(using ctx: Context): MatchType

  def MatchType_bound(self: MatchType)(using ctx: Context): Type
  def MatchType_scrutinee(self: MatchType)(using ctx: Context): Type
  def MatchType_cases(self: MatchType)(using ctx: Context): List[Type]

  /** Type of a by by name parameter */
  type ByNameType <: Type

  def ByNameType_TypeTest(using ctx: Context): TypeTest[TypeOrBounds, ByNameType]

  def ByNameType_apply(underlying: Type)(using ctx: Context): Type

  def ByNameType_underlying(self: ByNameType)(using ctx: Context): Type

  /** Type of a parameter reference */
  type ParamRef <: Type

  def ParamRef_TypeTest(using ctx: Context): TypeTest[TypeOrBounds, ParamRef]

  def ParamRef_binder(self: ParamRef)(using ctx: Context): LambdaType[TypeOrBounds]
  def ParamRef_paramNum(self: ParamRef)(using ctx: Context): Int

  /** Type of `this` */
  type ThisType <: Type

  def ThisType_TypeTest(using ctx: Context): TypeTest[TypeOrBounds, ThisType]

  def ThisType_tref(self: ThisType)(using ctx: Context): Type

  /** A type that is recursively defined `this` */
  type RecursiveThis <: Type

  def RecursiveThis_TypeTest(using ctx: Context): TypeTest[TypeOrBounds, RecursiveThis]

  def RecursiveThis_binder(self: RecursiveThis)(using ctx: Context): RecursiveType

  /** A type that is recursively defined */
  type RecursiveType <: Type

  def RecursiveType_TypeTest(using ctx: Context): TypeTest[TypeOrBounds, RecursiveType]

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

  // TODO can we add the bound back without an cake?
  // TODO is LambdaType really needed? ParamRefExtractor could be split into more precise extractors
  /** Common abstraction for lambda types (MethodType, PolyType and TypeLambda). */
  type LambdaType[ParamInfo /*<: TypeOrBounds*/] <: Type

  /** Type of the definition of a method taking a single list of parameters. It's return type may be a MethodType. */
  type MethodType <: LambdaType[Type]

  def MethodType_TypeTest(using ctx: Context): TypeTest[TypeOrBounds, MethodType]

  def MethodType_apply(paramNames: List[String])(paramInfosExp: MethodType => List[Type], resultTypeExp: MethodType => Type): MethodType

  def MethodType_isErased(self: MethodType): Boolean
  def MethodType_isImplicit(self: MethodType): Boolean
  def MethodType_param(self: MethodType, ids: Int)(using ctx: Context): Type
  def MethodType_paramNames(self: MethodType)(using ctx: Context): List[String]
  def MethodType_paramTypes(self: MethodType)(using ctx: Context): List[Type]
  def MethodType_resType(self: MethodType)(using ctx: Context): Type

  /** Type of the definition of a method taking a list of type parameters. It's return type may be a MethodType. */
  type PolyType <: LambdaType[TypeBounds]

  def PolyType_TypeTest(using ctx: Context): TypeTest[TypeOrBounds, PolyType]

  def PolyType_apply(paramNames: List[String])(paramBoundsExp: PolyType => List[TypeBounds], resultTypeExp: PolyType => Type)(using ctx: Context): PolyType

  def PolyType_param(self: PolyType, idx: Int)(using ctx: Context): Type
  def PolyType_paramNames(self: PolyType)(using ctx: Context): List[String]
  def PolyType_paramBounds(self: PolyType)(using ctx: Context): List[TypeBounds]
  def PolyType_resType(self: PolyType)(using ctx: Context): Type

  /** Type of the definition of a type lambda taking a list of type parameters. It's return type may be a TypeLambda. */
  type TypeLambda <: LambdaType[TypeBounds]

  def TypeLambda_TypeTest(using ctx: Context): TypeTest[TypeOrBounds, TypeLambda]

  def TypeLambda_apply(paramNames: List[String], boundsFn: TypeLambda => List[TypeBounds], bodyFn: TypeLambda => Type): TypeLambda

  def TypeLambda_paramNames(self: TypeLambda)(using ctx: Context): List[String]
  def TypeLambda_paramBounds(self: TypeLambda)(using ctx: Context): List[TypeBounds]
  def TypeLambda_param(self: TypeLambda, idx: Int)(using ctx: Context): Type
  def TypeLambda_resType(self: TypeLambda)(using ctx: Context): Type


  //////////////////////
  // IMPORT SELECTORS //
  //////////////////////

  /** Import selectors:
   *  * SimpleSelector: `.bar` in `import foo.bar`
   *  * RenameSelector: `.{bar => baz}` in `import foo.{bar => baz}`
   *  * OmitSelector: `.{bar => _}` in `import foo.{bar => _}`
   */
  type ImportSelector <: AnyRef

  type SimpleSelector <: ImportSelector

  def SimpleSelector_TypeTest(using ctx: Context): TypeTest[ImportSelector, SimpleSelector]

  def SimpleSelector_selection(self: SimpleSelector)(using ctx: Context): Id

  type RenameSelector <: ImportSelector

  def RenameSelector_TypeTest(using ctx: Context): TypeTest[ImportSelector, RenameSelector]

  def RenameSelector_from(self: RenameSelector)(using ctx: Context): Id
  def RenameSelector_to(self: RenameSelector)(using ctx: Context): Id

  type OmitSelector <: ImportSelector

  def OmitSelector_TypeTest(using ctx: Context): TypeTest[ImportSelector, OmitSelector]

  def SimpleSelector_omitted(self: OmitSelector)(using ctx: Context): Id


  /////////////////
  // IDENTIFIERS //
  /////////////////

  /** Untyped identifier */
  type Id <: AnyRef

  /** Position in the source code */
  def Id_pos(self: Id)(using ctx: Context): Position

  /** Name of the identifier */
  def Id_name(self: Id)(using ctx: Context): String


  ////////////////
  // SIGNATURES //
  ////////////////

  type Signature <: AnyRef

  def Signature_paramSigs(self: Signature): List[String | Int]

  def Signature_resultSig(self: Signature): String


  ///////////////
  // POSITIONS //
  ///////////////

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

  /////////////////
  // SOURCE FILE //
  /////////////////

  /** Scala source file */
  type SourceFile <: AnyRef

  /** Path to a source file */
  def SourceFile_jpath(self: SourceFile): java.nio.file.Path

  /** Content of a source file */
  def SourceFile_content(self: SourceFile): String


  //////////////
  // COMMENTS //
  //////////////

  /** Comment */
  type Comment <: AnyRef

  def Comment_raw(self: Comment): String
  def Comment_expanded(self: Comment): Option[String]
  def Comment_usecases(self: Comment): List[(String, Option[DefDef])]


  ///////////////
  // CONSTANTS //
  ///////////////

  /** Constant value represented as the constant itself */
  type Constant <: AnyRef

  def Constant_value(const: Constant): Any

  def matchConstant(constant: Constant): Option[Unit | Null | Int | Boolean | Byte | Short | Int | Long | Float | Double | Char | String | Type]
  def matchConstant_ClassTag(constant: Constant): Option[Type]

  def Constant_apply(x: Unit | Null | Int | Boolean | Byte | Short | Int | Long | Float | Double | Char | String | Type): Constant
  def Constant_ClassTag_apply(x: Type): Constant


  /////////////
  // SYMBOLS //
  /////////////

  /** Symbol of a definition.
   *  Then can be compared with == to know if the definition is the same.
   */
  type Symbol <: AnyRef

  /** Owner of this symbol. The owner is the symbol in which this symbol is defined. Throws if this symbol does not have an owner. */
  def Symbol_owner(self: Symbol)(using ctx: Context): Symbol

  /** Owner of this symbol. The owner is the symbol in which this symbol is defined. Returns `NoSymbol` if this symbol does not have an owner. */
  def Symbol_maybeOwner(self: Symbol)(using ctx: Context): Symbol

  /** Flags of this symbol */
  def Symbol_flags(self: Symbol)(using ctx: Context): Flags

  def Symbol_tree(self: Symbol)(using ctx: Context): Tree

  def Symbol_isLocalDummy(self: Symbol)(using ctx: Context): Boolean

  def Symbol_isRefinementClass(self: Symbol)(using ctx: Context): Boolean

  def Symbol_isAliasType(self: Symbol)(using ctx: Context): Boolean

  def Symbol_isAnonymousClass(self: Symbol)(using ctx: Context): Boolean

  def Symbol_isAnonymousFunction(self: Symbol)(using ctx: Context): Boolean

  def Symbol_isAbstractType(self: Symbol)(using ctx: Context): Boolean

  def Symbol_isClassConstructor(self: Symbol)(using ctx: Context): Boolean

  /** This symbol is private within the resulting type. */
  def Symbol_privateWithin(self: Symbol)(using ctx: Context): Option[Type]

  /** This symbol is protected within the resulting type. */
  def Symbol_protectedWithin(self: Symbol)(using ctx: Context): Option[Type]

  /** The name of this symbol. */
  def Symbol_name(self: Symbol)(using ctx: Context): String

  /** The full name of this symbol up to the root package. */
  def Symbol_fullName(self: Symbol)(using ctx: Context): String

  /** The position of this symbol */
  def Symbol_pos(self: Symbol)(using ctx: Context): Position

  def Symbol_localContext(self: Symbol)(using ctx: Context): Context

  /** The comment of the symbol */
  def Symbol_comment(self: Symbol)(using ctx: Context): Option[Comment]

  /** Annotations attached to this symbol */
  def Symbol_annots(self: Symbol)(using ctx: Context): List[Term]

  def Symbol_isDefinedInCurrentRun(self: Symbol)(using ctx: Context): Boolean

  /** Fields directly declared in the class */
  def Symbol_fields(self: Symbol)(using ctx: Context): List[Symbol]

  /** Field with the given name directly declared in the class */
  def Symbol_field(self: Symbol)(name: String)(using ctx: Context): Symbol

  /** Get non-private named methods defined directly inside the class */
  def Symbol_classMethod(self: Symbol)(name: String)(using ctx: Context): List[Symbol]

  /** Get all non-private methods defined directly inside the class, excluding constructors */
  def Symbol_classMethods(self: Symbol)(using ctx: Context): List[Symbol]

  /** Get named non-private methods declared or inherited */
  def Symbol_method(self: Symbol)(name: String)(using ctx: Context): List[Symbol]

  /** Get all non-private methods declared or inherited */
  def Symbol_methods(self: Symbol)(using ctx: Context): List[Symbol]

  /** Type member directly declared in the class */
  def Symbol_typeMembers(self: Symbol)(using ctx: Context): List[Symbol]

  /** Type member with the given name directly declared in the class */
  def Symbol_typeMember(self: Symbol)(name: String)(using ctx: Context): Symbol

  /** The symbols of each type parameter list and value parameter list of this
   *  method, or Nil if this isn't a method.
   */
  def Symbol_paramSymss(self: Symbol)(using ctx: Context): List[List[Symbol]]

  /** The primary constructor of a class or trait, `noSymbol` if not applicable. */
  def Symbol_primaryConstructor(self: Symbol)(using Context): Symbol

  /** Fields of a case class type -- only the ones declared in primary constructor */
  def Symbol_caseFields(self: Symbol)(using ctx: Context): List[Symbol]

  def Symbol_of(fullName: String)(using ctx: Context): Symbol

  def Symbol_newMethod(parent: Symbol, name: String, flags: Flags, tpe: Type, privateWithin: Symbol)(using ctx: Context): Symbol

  def Symbol_newVal(parent: Symbol, name: String, flags: Flags, tpe: Type, privateWithin: Symbol)(using ctx: Context): Symbol

  def Symbol_newBind(parent: Symbol, name: String, flags: Flags, tpe: Type)(using ctx: Context): Symbol

  def Symbol_isTypeParam(self: Symbol)(using ctx: Context): Boolean

  def Symbol_isPackageDef(symbol: Symbol)(using ctx: Context): Boolean

  /** Is this the definition of a type? */
  def Symbol_isType(symbol: Symbol)(using ctx: Context): Boolean

  /** Is this the definition of a term? */
  def Symbol_isTerm(symbol: Symbol)(using ctx: Context): Boolean

  /** Is this the definition of a ClassDef tree? */
  def Symbol_isClassDef(symbol: Symbol)(using ctx: Context): Boolean

  /** Is this the definition of a TypeDef tree? */
  def Symbol_isTypeDef(symbol: Symbol)(using ctx: Context): Boolean

  /** Is this the definition of a DefDef tree? */
  def Symbol_isDefDef(symbol: Symbol)(using ctx: Context): Boolean

  /** Is this the definition of a ValDef tree? */
  def Symbol_isValDef(symbol: Symbol)(using ctx: Context): Boolean

  /** Is this the definition of a Bind pattern? */
  def Symbol_isBind(symbol: Symbol)(using ctx: Context): Boolean

  /** Signature of this definition */
  def Symbol_signature(self: Symbol)(using ctx: Context): Signature

  /** The class symbol of the companion module class */
  def Symbol_moduleClass(self: Symbol)(using ctx: Context): Symbol

  /** The symbol of the companion class */
  def Symbol_companionClass(self: Symbol)(using ctx: Context): Symbol

  /** The symbol of the companion module */
  def Symbol_companionModule(self: Symbol)(using ctx: Context): Symbol

  def Symbol_noSymbol(using ctx: Context): Symbol

  /** Case class or case object children of a sealed trait */
  def Symbol_children(self: Symbol)(using ctx: Context): List[Symbol]


  ///////////
  // FLAGS //
  ///////////

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
  def Flags_HasDefault: Flags
  def Flags_StableRealizable: Flags
  def Flags_Param: Flags
  def Flags_ParamAccessor: Flags
  def Flags_Enum: Flags
  def Flags_ModuleClass: Flags
  def Flags_PrivateLocal: Flags
  def Flags_Package: Flags


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

  type ImplicitSearchResult <: AnyRef

  type ImplicitSearchSuccess <: ImplicitSearchResult
  def ImplicitSearchSuccess_TypeTest(using ctx: Context): TypeTest[ImplicitSearchResult, ImplicitSearchSuccess]
  def ImplicitSearchSuccess_tree(self: ImplicitSearchSuccess)(using ctx: Context): Term

  type ImplicitSearchFailure <: ImplicitSearchResult
  def ImplicitSearchFailure_TypeTest(using ctx: Context): TypeTest[ImplicitSearchResult, ImplicitSearchFailure]
  def ImplicitSearchFailure_explanation(self: ImplicitSearchFailure)(using ctx: Context): String

  type DivergingImplicit <: ImplicitSearchFailure
  def DivergingImplicit_TypeTest(using ctx: Context): TypeTest[ImplicitSearchResult, DivergingImplicit]

  type NoMatchingImplicits <: ImplicitSearchFailure
  def NoMatchingImplicits_TypeTest(using ctx: Context): TypeTest[ImplicitSearchResult, NoMatchingImplicits]

  type AmbiguousImplicits <: ImplicitSearchFailure
  def AmbiguousImplicits_TypeTest(using ctx: Context): TypeTest[ImplicitSearchResult, AmbiguousImplicits]

  /** Find an implicit of type `T` in the current scope given by `ctx`.
   *  Return an `ImplicitSearchResult`.
   *
   *  @param tpe type of the implicit parameter
   *  @param ctx current context
   */
  def searchImplicit(tpe: Type)(using ctx: Context): ImplicitSearchResult

  /** Returns Some with a beta-reduced application or None */
  def betaReduce(tree: Term)(using Context): Option[Term]

  def lambdaExtractor(term: Term, paramTypes: List[Type])(using ctx: Context): Option[List[Term] => Term]

  def compilerId: Int

}
