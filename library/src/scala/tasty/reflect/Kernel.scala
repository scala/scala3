package scala.tasty.reflect

/** Tasty reflect abstract types
 *
 *  ```none
 *
 *  +- Tree -+- PackageClause
 *           +- Import
 *           +- Statement -+- Definition --+- PackageDef
 *                         |               +- ClassDef
 *                         |               +- TypeDef
 *                         |               +- DefDef
 *                         |               +- ValDef
 *                         |
 *                         +- Term --------+- Ref -+- Ident
 *                                         |       +- Select
 *                                         |
 *                                         +- Literal
 *                                         +- This
 *                                         +- New
 *                                         +- NamedArg
 *                                         +- Apply
 *                                         +- TypeApply
 *                                         +- Super
 *                                         +- Typed
 *                                         +- Assign
 *                                         +- Block
 *                                         +- Lambda
 *                                         +- If
 *                                         +- Match
 *                                         +- Try
 *                                         +- Return
 *                                         +- Repeated
 *                                         +- Inlined
 *                                         +- SelectOuter
 *                                         +- While
 *
 *
 *                         +- TypeTree ----+- Inferred
 *                         |               +- Ident
 *                         |               +- Select
 *                         |               +- Project
 *                         |               +- Singleton
 *  +- TypeOrBoundsTree ---+               +- Refined
 *                         |               +- Applied
 *                         |               +- Annotated
 *                         |               +- MatchType
 *                         |               +- ByName
 *                         |               +- LambdaTypeTree
 *                         |               +- TypeBind
 *                         |               +- TypeBlock
 *                         |
 *                         +- TypeBoundsTree
 *                         +- WildcardTypeTree
 *
 *  +- CaseDef
 *  +- TypeCaseDef
 *
 *  +- Pattern --+- Value
 *               +- Bind
 *               +- Unapply
 *               +- Alternatives
 *               +- TypeTest
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
 *  +- Symbol --+- PackageSymbol
 *              +- ClassSymbol
 *              +- TypeSymbol
 *              +- DefSymbol
 *              +- ValSymbol
 *              +- BindSymbol
 *              +- NoSymbol
 *
 *  +- Flags
 *
 *  Aliases:
 *   # TermOrTypeTree = Term | TypeTree
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

  //
  // Settings
  //

  /** Settings */
  type Settings <: AnyRef

  def Settings_color(self: Settings): Boolean

  //
  // TREES
  //

  // TODO: When bootstrapped, remove and use `Term | TypeTree` type directly in other files
  /** Workaround missing `|` types in Scala 2 to represent `Term | TypeTree` */
  type TermOrTypeTree /* Term | TypeTree */ <: AnyRef

  /** Tree representing code written in the source */
  type Tree <: AnyRef

  def Tree_pos(self: Tree)(implicit ctx: Context): Position
  def Tree_symbol(self: Tree)(implicit ctx: Context): Symbol

  /** Tree representing a pacakage clause in the source code */
  type PackageClause <: Tree

  def isPackageClause(tree: Tree)(implicit ctx: Context): Option[PackageClause]

  def PackageClause_pid(self: PackageClause)(implicit ctx: Context): Term_Ref
  def PackageClause_stats(self: PackageClause)(implicit ctx: Context): List[Tree]

  def PackageClause_apply(pid: Term_Ref, stats: List[Tree])(implicit ctx: Context): PackageClause

  def PackageClause_copy(original: PackageClause)(pid: Term_Ref, stats: List[Tree])(implicit ctx: Context): PackageClause

  /** Tree representing a statement in the source code */
  type Statement <: Tree

  def isStatement(tree: Tree)(implicit ctx: Context): Option[Statement]

  /** Tree representing an import in the source code */
  type Import <: Statement

  def isImport(tree: Tree)(implicit ctx: Context): Option[Import]

  def Import_impliedOnly(self: Import): Boolean
  def Import_expr(self: Import)(implicit ctx: Context): Term
  def Import_selectors(self: Import)(implicit ctx: Context): List[ImportSelector]

  def Import_apply(impliedOnly: Boolean, expr: Term, selectors: List[ImportSelector])(implicit ctx: Context): Import

  def Import_copy(original: Import)(impliedOnly: Boolean, expr: Term, selectors: List[ImportSelector])(implicit ctx: Context): Import

  /** Tree representing a definition in the source code. It can be `PackageDef`, `ClassDef`, `TypeDef`, `DefDef` or `ValDef` */
  type Definition <: Statement

  def isDefinition(tree: Tree)(implicit ctx: Context): Option[Definition]

  def Definition_name(self: Definition)(implicit ctx: Context): String

  /** Tree representing a package definition. This includes definitions in all source files */
  type PackageDef <: Definition

  def isPackageDef(tree: Tree)(implicit ctx: Context): Option[PackageDef]

  def PackageDef_owner(self: PackageDef)(implicit ctx: Context): PackageDef
  def PackageDef_members(self: PackageDef)(implicit ctx: Context): List[Statement]
  def PackageDef_symbol(self: PackageDef)(implicit ctx: Context): PackageSymbol

  /** Tree representing a class definition. This includes annonymus class definitions and the class of a module object */
  type ClassDef <: Definition

  def isClassDef(tree: Tree)(implicit ctx: Context): Option[ClassDef]

  def ClassDef_constructor(self: ClassDef)(implicit ctx: Context): DefDef
  def ClassDef_parents(self: ClassDef)(implicit ctx: Context): List[TermOrTypeTree]
  def ClassDef_derived(self: ClassDef)(implicit ctx: Context): List[TypeTree]
  def ClassDef_self(self: ClassDef)(implicit ctx: Context): Option[ValDef]
  def ClassDef_body(self: ClassDef)(implicit ctx: Context): List[Statement]
  def ClassDef_symbol(self: ClassDef)(implicit ctx: Context): ClassSymbol

  def ClassDef_copy(original: ClassDef)(name: String, constr: DefDef, parents: List[TermOrTypeTree], derived: List[TypeTree], selfOpt: Option[ValDef], body: List[Statement])(implicit ctx: Context): ClassDef

  /** Tree representing a type (paramter or member) definition in the source code */
  type TypeDef <: Definition

  def isTypeDef(tree: Tree)(implicit ctx: Context): Option[TypeDef]

  def TypeDef_rhs(self: TypeDef)(implicit ctx: Context): TypeOrBoundsTree
  def TypeDef_symbol(self: TypeDef)(implicit ctx: Context): TypeSymbol

  def TypeDef_apply(symbol: TypeSymbol)(implicit ctx: Context): TypeDef
  def TypeDef_copy(original: TypeDef)(name: String, rhs: TypeOrBoundsTree)(implicit ctx: Context): TypeDef

  /** Tree representing a method definition in the source code */
  type DefDef <: Definition

  def isDefDef(tree: Tree)(implicit ctx: Context): Option[DefDef]

  def DefDef_typeParams(self: DefDef)(implicit ctx: Context): List[TypeDef]
  def DefDef_paramss(self: DefDef)(implicit ctx: Context): List[List[ValDef]]
  def DefDef_returnTpt(self: DefDef)(implicit ctx: Context): TypeTree
  def DefDef_rhs(self: DefDef)(implicit ctx: Context): Option[Term]
  def DefDef_symbol(self: DefDef)(implicit ctx: Context): DefSymbol

  def DefDef_apply(symbol: DefSymbol, rhsFn: List[Type] => List[List[Term]] => Option[Term])(implicit ctx: Context): DefDef
  def DefDef_copy(original: DefDef)(name: String, typeParams: List[TypeDef], paramss: List[List[ValDef]], tpt: TypeTree, rhs: Option[Term])(implicit ctx: Context): DefDef

  /** Tree representing a value definition in the source code This inclues `val`, `lazy val`, `var`, `object` and parameter defintions. */
  type ValDef <: Definition

  def isValDef(tree: Tree)(implicit ctx: Context): Option[ValDef]

  def ValDef_tpt(self: ValDef)(implicit ctx: Context): TypeTree
  def ValDef_rhs(self: ValDef)(implicit ctx: Context): Option[Term]
  def ValDef_symbol(self: ValDef)(implicit ctx: Context): ValSymbol

  def ValDef_apply(symbol: ValSymbol, rhs: Option[Term])(implicit ctx: Context): ValDef
  def ValDef_copy(original: ValDef)(name: String, tpt: TypeTree, rhs: Option[Term])(implicit ctx: Context): ValDef

  /** Tree representing an expression in the source code */
  type Term <: Statement

  def isTerm(tree: Tree)(implicit ctx: Context): Option[Term]

  def isTermNotTypeTree(termOrTypeTree: TermOrTypeTree)(implicit ctx: Context): Option[Term]

  def Term_pos(self: Term)(implicit ctx: Context): Position
  def Term_tpe(self: Term)(implicit ctx: Context): Type
  def Term_underlyingArgument(self: Term)(implicit ctx: Context): Term
  def Term_underlying(self: Term)(implicit ctx: Context): Term

  /** Tree representing a reference to definition */
  type Term_Ref <: Term

  def Term_Ref_apply(sym: Symbol)(implicit ctx: Context): Term_Ref

  /** Tree representing a reference to definition with a given name */
  type Term_Ident <: Term_Ref

  def isTerm_Ident(tree: Tree)(implicit ctx: Context): Option[Term_Ident]

  def Term_Ident_name(self: Term_Ident)(implicit ctx: Context): String

  def Term_Ident_apply(tmref: TermRef)(implicit ctx: Context): Term
  def Term_Ident_copy(original: Tree)(name: String)(implicit ctx: Context): Term_Ident

  /** Tree representing a selection of definition with a given name on a given prefix */
  type Term_Select <: Term_Ref

  def isTerm_Select(tree: Tree)(implicit ctx: Context): Option[Term_Select]

  def Term_Select_qualifier(self: Term_Select)(implicit ctx: Context): Term
  def Term_Select_name(self: Term_Select)(implicit ctx: Context): String
  def Term_Select_signature(self: Term_Select)(implicit ctx: Context): Option[Signature]

  def Term_Select_unique(qualifier: Term, name: String)(implicit ctx: Context): Term_Select
  // TODO rename, this returns an Apply and not a Select
  def Term_Select_overloaded(qualifier: Term, name: String, targs: List[Type], args: List[Term])(implicit ctx: Context): Term_Apply
  def Term_Select_copy(original: Tree)(qualifier: Term, name: String)(implicit ctx: Context): Term_Select

  /** Tree representing a literal value in the source code */
  type Term_Literal <: Term

  def isTerm_Literal(tree: Tree)(implicit ctx: Context): Option[Term_Literal]

  def Term_Literal_constant(self: Term_Literal)(implicit ctx: Context): Constant

  def Term_Literal_apply(constant: Constant)(implicit ctx: Context): Term_Literal
  def Term_Literal_copy(original: Tree)(constant: Constant)(implicit ctx: Context): Term_Literal

  /** Tree representing `this` in the source code */
  type Term_This <: Term

  def isTerm_This(tree: Tree)(implicit ctx: Context): Option[Term_This]

  def Term_This_id(self: Term_This)(implicit ctx: Context): Option[Id]

  def Term_This_apply(cls: ClassSymbol)(implicit ctx: Context): Term_This
  def Term_This_copy(original: Tree)(qual: Option[Id])(implicit ctx: Context): Term_This

  /** Tree representing `new` in the source code */
  type Term_New <: Term

  def isTerm_New(tree: Tree)(implicit ctx: Context): Option[Term_New]

  def Term_New_tpt(self: Term_New)(implicit ctx: Context): TypeTree

  def Term_New_apply(tpt: TypeTree)(implicit ctx: Context): Term_New
  def Term_New_copy(original: Tree)(tpt: TypeTree)(implicit ctx: Context): Term_New

  /** Tree representing an argument passed with an explicit name. Such as `arg1 = x` in `foo(arg1 = x)` */
  type Term_NamedArg <: Term

  def isTerm_NamedArg(tree: Tree)(implicit ctx: Context): Option[Term_NamedArg]

  def Term_NamedArg_name(self: Term_NamedArg)(implicit ctx: Context): String
  def Term_NamedArg_value(self: Term_NamedArg)(implicit ctx: Context): Term

  def Term_NamedArg_apply(name: String, arg: Term)(implicit ctx: Context): Term_NamedArg
  def Term_NamedArg_copy(tree: Term_NamedArg)(name: String, arg: Term)(implicit ctx: Context): Term_NamedArg

  /** Tree an application of arguments. It represents a single list of arguments, multiple argument lists will have nested `Apply`s */
  type Term_Apply <: Term

  def isTerm_Apply(tree: Tree)(implicit ctx: Context): Option[Term_Apply]

  def Term_Apply_fun(self: Term_Apply)(implicit ctx: Context): Term
  def Term_Apply_args(self: Term_Apply)(implicit ctx: Context): List[Term]

  def Term_Apply_apply(fn: Term, args: List[Term])(implicit ctx: Context): Term_Apply
  def Term_Apply_copy(original: Tree)(fun: Term, args: List[Term])(implicit ctx: Context): Term_Apply

  /** Tree an application of type arguments */
  type Term_TypeApply <: Term

  def isTerm_TypeApply(tree: Tree)(implicit ctx: Context): Option[Term_TypeApply]

  def Term_TypeApply_fun(self: Term_TypeApply)(implicit ctx: Context): Term
  def Term_TypeApply_args(self: Term_TypeApply)(implicit ctx: Context): List[TypeTree]

  def Term_TypeApply_apply(fn: Term, args: List[TypeTree])(implicit ctx: Context): Term_TypeApply
  def Term_TypeApply_copy(original: Tree)(fun: Term, args: List[TypeTree])(implicit ctx: Context): Term_TypeApply

  /** Tree representing `super` in the source code */
  type Term_Super <: Term

  def isTerm_Super(tree: Tree)(implicit ctx: Context): Option[Term_Super]

  def Term_Super_qualifier(self: Term_Super)(implicit ctx: Context): Term
  def Term_Super_id(self: Term_Super)(implicit ctx: Context): Option[Id]

  def Term_Super_apply(qual: Term, mix: Option[Id])(implicit ctx: Context): Term_Super
  def Term_Super_copy(original: Tree)(qual: Term, mix: Option[Id])(implicit ctx: Context): Term_Super

  /** Tree representing a type ascription `x: T` in the source code */
  type Term_Typed <: Term

  def isTerm_Typed(tree: Tree)(implicit ctx: Context): Option[Term_Typed]

  def Term_Typed_expr(self: Term_Typed)(implicit ctx: Context): Term
  def Term_Typed_tpt(self: Term_Typed)(implicit ctx: Context): TypeTree

  def Term_Typed_apply(expr: Term, tpt: TypeTree)(implicit ctx: Context): Term_Typed
  def Term_Typed_copy(original: Tree)(expr: Term, tpt: TypeTree)(implicit ctx: Context): Term_Typed

  /** Tree representing an assignment `x = y` in the source code */
  type Term_Assign <: Term

  def isTerm_Assign(tree: Tree)(implicit ctx: Context): Option[Term_Assign]

  def Term_Assign_lhs(self: Term_Assign)(implicit ctx: Context): Term
  def Term_Assign_rhs(self: Term_Assign)(implicit ctx: Context): Term

  def Term_Assign_apply(lhs: Term, rhs: Term)(implicit ctx: Context): Term_Assign
  def Term_Assign_copy(original: Tree)(lhs: Term, rhs: Term)(implicit ctx: Context): Term_Assign

  /** Tree representing a block `{ ... }` in the source code */
  type Term_Block <: Term

  def isTerm_Block(tree: Tree)(implicit ctx: Context): Option[Term_Block]

  def Term_Block_statements(self: Term_Block)(implicit ctx: Context): List[Statement]
  def Term_Block_expr(self: Term_Block)(implicit ctx: Context): Term

  def Term_Block_apply(stats: List[Statement], expr: Term)(implicit ctx: Context): Term_Block
  def Term_Block_copy(original: Tree)(stats: List[Statement], expr: Term)(implicit ctx: Context): Term_Block

  /** Tree representing a lambda `(...) => ...` in the source code */
  type Term_Lambda <: Term

  def isTerm_Lambda(tree: Tree)(implicit ctx: Context): Option[Term_Lambda]

  def Term_Lambda_meth(self: Term_Lambda)(implicit ctx: Context): Term
  def Term_Lambda_tptOpt(self: Term_Lambda)(implicit ctx: Context): Option[TypeTree]

  def Term_Lambda_apply(meth: Term, tpt: Option[TypeTree])(implicit ctx: Context): Term_Lambda
  def Term_Lambda_copy(original: Tree)(meth: Tree, tpt: Option[TypeTree])(implicit ctx: Context): Term_Lambda

  /** Tree representing an if/then/else `if (...) ... else ...` in the source code */
  type Term_If <: Term

  def isTerm_If(tree: Tree)(implicit ctx: Context): Option[Term_If]

  def Term_If_cond(self: Term_If)(implicit ctx: Context): Term
  def Term_If_thenp(self: Term_If)(implicit ctx: Context): Term
  def Term_If_elsep(self: Term_If)(implicit ctx: Context): Term

  def Term_If_apply(cond: Term, thenp: Term, elsep: Term)(implicit ctx: Context): Term_If
  def Term_If_copy(original: Tree)(cond: Term, thenp: Term, elsep: Term)(implicit ctx: Context): Term_If

  /** Tree representing a pattern match `x match  { ... }` in the source code */
  type Term_Match <: Term

  def isTerm_Match(tree: Tree)(implicit ctx: Context): Option[Term_Match]

  def Term_Match_scrutinee(self: Term_Match)(implicit ctx: Context): Term
  def Term_Match_cases(self: Term_Match)(implicit ctx: Context): List[CaseDef]

  def Term_Match_apply(selector: Term, cases: List[CaseDef])(implicit ctx: Context): Term_Match
  def Term_Match_copy(original: Tree)(selector: Term, cases: List[CaseDef])(implicit ctx: Context): Term_Match

  /** Tree representing a tyr catch `try x catch { ... } finally { ... }` in the source code */
  type Term_Try <: Term

  def isTerm_Try(tree: Tree)(implicit ctx: Context): Option[Term_Try]

  def Term_Try_body(self: Term_Try)(implicit ctx: Context): Term
  def Term_Try_cases(self: Term_Try)(implicit ctx: Context): List[CaseDef]
  def Term_Try_finalizer(self: Term_Try)(implicit ctx: Context): Option[Term]

  def Term_Try_apply(expr: Term, cases: List[CaseDef], finalizer: Option[Term])(implicit ctx: Context): Term_Try
  def Term_Try_copy(original: Tree)(expr: Term, cases: List[CaseDef], finalizer: Option[Term])(implicit ctx: Context): Term_Try

  /** Tree representing a `return` in the source code */
  type Term_Return <: Term

  def isTerm_Return(tree: Tree)(implicit ctx: Context): Option[Term_Return]

  def Term_Return_expr(self: Term_Return)(implicit ctx: Context): Term

  def Term_Return_apply(expr: Term)(implicit ctx: Context): Term_Return
  def Term_Return_copy(original: Tree)(expr: Term)(implicit ctx: Context): Term_Return

  /** Tree representing a variable argument list in the source code */
  type Term_Repeated <: Term

  def isTerm_Repeated(tree: Tree)(implicit ctx: Context): Option[Term_Repeated]

  def Term_Repeated_elems(self: Term_Repeated)(implicit ctx: Context): List[Term]
  def Term_Repeated_elemtpt(self: Term_Repeated)(implicit ctx: Context): TypeTree

  def Term_Repeated_apply(elems: List[Term], elemtpt: TypeTree)(implicit ctx: Context): Term_Repeated
  def Term_Repeated_copy(original: Tree)(elems: List[Term], elemtpt: TypeTree)(implicit ctx: Context): Term_Repeated

  /** Tree representing the scope of an inlined tree */
  type Term_Inlined <: Term

  def isTerm_Inlined(tree: Tree)(implicit ctx: Context): Option[Term_Inlined]

  def Term_Inlined_call(self: Term_Inlined)(implicit ctx: Context): Option[TermOrTypeTree]
  def Term_Inlined_bindings(self: Term_Inlined)(implicit ctx: Context): List[Definition]
  def Term_Inlined_body(self: Term_Inlined)(implicit ctx: Context): Term

  def Term_Inlined_apply(call: Option[TermOrTypeTree], bindings: List[Definition], expansion: Term)(implicit ctx: Context): Term_Inlined
  def Term_Inlined_copy(original: Tree)(call: Option[TermOrTypeTree], bindings: List[Definition], expansion: Term)(implicit ctx: Context): Term_Inlined

  /** Tree representing a selection of definition with a given name on a given prefix and number of nested scopes of inlined trees */
  type Term_SelectOuter <: Term

  def isTerm_SelectOuter(tree: Tree)(implicit ctx: Context): Option[Term_SelectOuter]

  def Term_SelectOuter_qualifier(self: Term_SelectOuter)(implicit ctx: Context): Term
  def Term_SelectOuter_level(self: Term_SelectOuter)(implicit ctx: Context): Int
  def Term_SelectOuter_tpe(self: Term_SelectOuter)(implicit ctx: Context): Type

  def Term_SelectOuter_apply(qualifier: Term, name: String, levels: Int)(implicit ctx: Context): Term_SelectOuter
  def Term_SelectOuter_copy(original: Tree)(qualifier: Term, name: String, levels: Int)(implicit ctx: Context): Term_SelectOuter

  /** Tree representing a while loop */
  type Term_While <: Term

  def isTerm_While(tree: Tree)(implicit ctx: Context): Option[Term_While]

  def Term_While_cond(self: Term_While)(implicit ctx: Context): Term
  def Term_While_body(self: Term_While)(implicit ctx: Context): Term

  def Term_While_apply(cond: Term, body: Term)(implicit ctx: Context): Term_While
  def Term_While_copy(original: Tree)(cond: Term, body: Term)(implicit ctx: Context): Term_While

  //
  // CASES
  //
  
  /** Branch of a pattern match or catch clause */
  type CaseDef <: AnyRef

  def CaseDef_pattern(self: CaseDef)(implicit ctx: Context): Pattern
  def CaseDef_guard(self: CaseDef)(implicit ctx: Context): Option[Term]
  def CaseDef_rhs(self: CaseDef)(implicit ctx: Context): Term

  def CaseDef_module_apply(pattern: Pattern, guard: Option[Term], body: Term)(implicit ctx: Context): CaseDef
  def CaseDef_module_copy(original: CaseDef)(pattern: Pattern, guard: Option[Term], body: Term)(implicit ctx: Context): CaseDef

  /** Branch of a type pattern match */
  type TypeCaseDef <: AnyRef

  def TypeCaseDef_pattern(self: TypeCaseDef)(implicit ctx: Context): TypeTree
  def TypeCaseDef_rhs(self: TypeCaseDef)(implicit ctx: Context): TypeTree

  def TypeCaseDef_module_apply(pattern: TypeTree, body: TypeTree)(implicit ctx: Context): TypeCaseDef
  def TypeCaseDef_module_copy(original: TypeCaseDef)(pattern: TypeTree, body: TypeTree)(implicit ctx: Context): TypeCaseDef

  //
  // PATTERNS
  //
  
  /** Pattern tree of the pattern part of a CaseDef */
  type Pattern <: AnyRef

  def Pattern_pos(self: Pattern)(implicit ctx: Context): Position
  def Pattern_tpe(self: Pattern)(implicit ctx: Context): Type
  def Pattern_symbol(self: Pattern)(implicit ctx: Context): Symbol

  /** Pattern representing a value. This includes `1`, ```x``` and `_` */
  type Value <: Pattern

  def isPattern_Value(pattern: Pattern): Option[Value]

  def Pattern_Value_value(self: Value)(implicit ctx: Context): Term

  def Pattern_Value_module_apply(term: Term)(implicit ctx: Context): Value
  def Pattern_Value_module_copy(original: Value)(term: Term)(implicit ctx: Context): Value

  /** Pattern representing a `_ @ _` binding. */
  type Bind <: Pattern

  def isPattern_Bind(x: Pattern)(implicit ctx: Context): Option[Bind]

  def Pattern_Bind_name(self: Bind)(implicit ctx: Context): String

  def Pattern_Bind_pattern(self: Bind)(implicit ctx: Context): Pattern

  def Pattern_Bind_module_copy(original: Bind)(name: String, pattern: Pattern)(implicit ctx: Context): Bind

  /** Pattern representing a `Xyz(...)` unapply. */
  type Unapply <: Pattern

  def isPattern_Unapply(pattern: Pattern)(implicit ctx: Context): Option[Unapply]

  def Pattern_Unapply_fun(self: Unapply)(implicit ctx: Context): Term

  def Pattern_Unapply_implicits(self: Unapply)(implicit ctx: Context): List[Term]

  def Pattern_Unapply_patterns(self: Unapply)(implicit ctx: Context): List[Pattern]

  def Pattern_Unapply_module_copy(original: Unapply)(fun: Term, implicits: List[Term], patterns: List[Pattern])(implicit ctx: Context): Unapply

  /** Pattern representing `X | Y | ...` alternatives. */
  type Alternatives <: Pattern

  def isPattern_Alternatives(pattern: Pattern)(implicit ctx: Context): Option[Alternatives]

  def Pattern_Alternatives_patterns(self: Alternatives)(implicit ctx: Context): List[Pattern]

  def Pattern_Alternatives_module_apply(patterns: List[Pattern])(implicit ctx: Context): Alternatives
  def Pattern_Alternatives_module_copy(original: Alternatives)(patterns: List[Pattern])(implicit ctx: Context): Alternatives

  /** Pattern representing a `x: Y` type test. */
  type TypeTest <: Pattern

  def isPattern_TypeTest(pattern: Pattern)(implicit ctx: Context): Option[TypeTest]

  def Pattern_TypeTest_tpt(self: TypeTest)(implicit ctx: Context): TypeTree

  def Pattern_TypeTest_module_apply(tpt: TypeTree)(implicit ctx: Context): TypeTest
  def Pattern_TypeTest_module_copy(original: TypeTest)(tpt: TypeTree)(implicit ctx: Context): TypeTest

  //
  // TYPE TREES
  //
  
  /** Type tree representing a type or a bounds written in the source */
  type TypeOrBoundsTree <: AnyRef

  def TypeOrBoundsTree_tpe(self: TypeOrBoundsTree)(implicit ctx: Context): Type

  /** Type tree representing a type written in the source */
  type TypeTree <: TypeOrBoundsTree

  def isTypeTree(x: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeTree]
  def isTypeTreeNotTerm(termOrTypeTree: TermOrTypeTree)(implicit ctx: Context): Option[TypeTree]

  def TypeTree_pos(self: TypeTree)(implicit ctx: Context): Position
  def TypeTree_symbol(self: TypeTree)(implicit ctx: Context): Symbol
  def TypeTree_tpe(self: TypeTree)(implicit ctx: Context): Type

  /** Type tree representing an inferred type */
  type TypeTree_Inferred <: TypeTree

  def isTypeTree_Inferred(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeTree_Inferred]

  def TypeTree_Inferred_apply(tpe: Type)(implicit ctx: Context): TypeTree_Inferred

  /** Type tree representing a reference to definition with a given name */
  type TypeTree_Ident <: TypeTree

  def isTypeTree_Ident(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeTree_Ident]

  def TypeTree_Ident_name(self: TypeTree_Ident)(implicit ctx: Context): String

  def TypeTree_Ident_copy(original: TypeTree_Ident)(name: String)(implicit ctx: Context): TypeTree_Ident

  /** Type tree representing a selection of definition with a given name on a given term prefix */
  type TypeTree_Select <: TypeTree

  def isTypeTree_Select(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeTree_Select]

  def TypeTree_Select_qualifier(self: TypeTree_Select)(implicit ctx: Context): Term
  def TypeTree_Select_name(self: TypeTree_Select)(implicit ctx: Context): String

  def TypeTree_Select_apply(qualifier: Term, name: String)(implicit ctx: Context): TypeTree_Select
  def TypeTree_Select_copy(original: TypeTree_Select)(qualifier: Term, name: String)(implicit ctx: Context): TypeTree_Select

  /** Type tree representing a selection of definition with a given name on a given type prefix */
  type TypeTree_Projection <: TypeTree

  def isTypeTree_Projection(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeTree_Projection]

  def TypeTree_Projection_qualifier(self: TypeTree_Projection)(implicit ctx: Context): TypeTree
  def TypeTree_Projection_name(self: TypeTree_Projection)(implicit ctx: Context): String

  def TypeTree_Projection_copy(original: TypeTree_Projection)(qualifier: TypeTree, name: String)(implicit ctx: Context): TypeTree_Projection

  /** Type tree representing a singleton type */
  type TypeTree_Singleton <: TypeTree

  def isTypeTree_Singleton(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeTree_Singleton]

  def TypeTree_Singleton_ref(self: TypeTree_Singleton)(implicit ctx: Context): Term

  def TypeTree_Singleton_apply(ref: Term)(implicit ctx: Context): TypeTree_Singleton
  def TypeTree_Singleton_copy(original: TypeTree_Singleton)(ref: Term)(implicit ctx: Context): TypeTree_Singleton

  /** Type tree representing a type refinement */
  type TypeTree_Refined <: TypeTree

  def isTypeTree_Refined(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeTree_Refined]

  def TypeTree_Refined_tpt(self: TypeTree_Refined)(implicit ctx: Context): TypeTree
  def TypeTree_Refined_refinements(self: TypeTree_Refined)(implicit ctx: Context): List[Definition]

  def TypeTree_Refined_copy(original: TypeTree_Refined)(tpt: TypeTree, refinements: List[Definition])(implicit ctx: Context): TypeTree_Refined

  /** Type tree representing a type application */
  type TypeTree_Applied <: TypeTree

  def isTypeTree_Applied(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeTree_Applied]

  def TypeTree_Applied_tpt(self: TypeTree_Applied)(implicit ctx: Context): TypeTree
  def TypeTree_Applied_args(self: TypeTree_Applied)(implicit ctx: Context): List[TypeOrBoundsTree]

  def TypeTree_Applied_apply(tpt: TypeTree, args: List[TypeOrBoundsTree])(implicit ctx: Context): TypeTree_Applied
  def TypeTree_Applied_copy(original: TypeTree_Applied)(tpt: TypeTree, args: List[TypeOrBoundsTree])(implicit ctx: Context): TypeTree_Applied

  /** Type tree representing an annotated type */
  type TypeTree_Annotated <: TypeTree

  def isTypeTree_Annotated(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeTree_Annotated]

  def TypeTree_Annotated_arg(self: TypeTree_Annotated)(implicit ctx: Context): TypeTree
  def TypeTree_Annotated_annotation(self: TypeTree_Annotated)(implicit ctx: Context): Term

  def TypeTree_Annotated_apply(arg: TypeTree, annotation: Term)(implicit ctx: Context): TypeTree_Annotated
  def TypeTree_Annotated_copy(original: TypeTree_Annotated)(arg: TypeTree, annotation: Term)(implicit ctx: Context): TypeTree_Annotated

  /** Type tree representing a type match */
  type TypeTree_MatchType <: TypeTree

  def isTypeTree_MatchType(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeTree_MatchType]

  def TypeTree_MatchType_bound(self: TypeTree_MatchType)(implicit ctx: Context): Option[TypeTree]
  def TypeTree_MatchType_selector(self: TypeTree_MatchType)(implicit ctx: Context): TypeTree
  def TypeTree_MatchType_cases(self: TypeTree_MatchType)(implicit ctx: Context): List[TypeCaseDef]

  def TypeTree_MatchType_apply(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef])(implicit ctx: Context): TypeTree_MatchType
  def TypeTree_MatchType_copy(original: TypeTree_MatchType)(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef])(implicit ctx: Context): TypeTree_MatchType

  /** Type tree representing a by name parameter */
  type TypeTree_ByName <: TypeTree

  def TypeTree_ByName_result(self: TypeTree_ByName)(implicit ctx: Context): TypeTree

  def isTypeTree_ByName(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeTree_ByName]

  def TypeTree_ByName_apply(result: TypeTree)(implicit ctx: Context): TypeTree_ByName
  def TypeTree_ByName_copy(original: TypeTree_ByName)(result: TypeTree)(implicit ctx: Context): TypeTree_ByName

  /** Type tree representing a lambda abstraction type */
  type TypeTree_LambdaTypeTree <: TypeTree

  def isTypeTree_LambdaTypeTree(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeTree_LambdaTypeTree]

  def TypeTree_LambdaTypeTree_tparams(self: TypeTree_LambdaTypeTree)(implicit ctx: Context): List[TypeDef]
  def TypeTree_LambdaTypeTree_body(self: TypeTree_LambdaTypeTree)(implicit ctx: Context): TypeOrBoundsTree

  def TypeTree_LambdaTypeTree_apply(tparams: List[TypeDef], body: TypeOrBoundsTree)(implicit ctx: Context): TypeTree_LambdaTypeTree
  def TypeTree_LambdaTypeTree_copy(original: TypeTree_LambdaTypeTree)(tparams: List[TypeDef], body: TypeOrBoundsTree)(implicit ctx: Context): TypeTree_LambdaTypeTree

  /** Type tree representing a type binding */
  type TypeTree_TypeBind <: TypeTree

  def isTypeTree_TypeBind(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeTree_TypeBind]

  def TypeTree_TypeBind_name(self: TypeTree_TypeBind)(implicit ctx: Context): String
  def TypeTree_TypeBind_body(self: TypeTree_TypeBind)(implicit ctx: Context): TypeOrBoundsTree

  def TypeTree_TypeBind_copy(original: TypeTree_TypeBind)(name: String, tpt: TypeOrBoundsTree)(implicit ctx: Context): TypeTree_TypeBind

  /** Type tree within a block with aliases `{ type U1 = ... ; T[U1, U2] }` */
  type TypeTree_TypeBlock <: TypeTree

  def isTypeTree_TypeBlock(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeTree_TypeBlock]

  def TypeTree_TypeBlock_aliases(self: TypeTree_TypeBlock)(implicit ctx: Context): List[TypeDef]
  def TypeTree_TypeBlock_tpt(self: TypeTree_TypeBlock)(implicit ctx: Context): TypeTree

  def TypeTree_TypeBlock_apply(aliases: List[TypeDef], tpt: TypeTree)(implicit ctx: Context): TypeTree_TypeBlock
  def TypeTree_TypeBlock_copy(original: TypeTree_TypeBlock)(aliases: List[TypeDef], tpt: TypeTree)(implicit ctx: Context): TypeTree_TypeBlock

  /** Type tree representing a type bound written in the source */
  type TypeBoundsTree <: TypeOrBoundsTree

  def isTypeBoundsTree(x: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeBoundsTree]

  def TypeBoundsTree_tpe(self: TypeBoundsTree)(implicit ctx: Context): TypeBounds
  def TypeBoundsTree_low(self: TypeBoundsTree)(implicit ctx: Context): TypeTree
  def TypeBoundsTree_hi(self: TypeBoundsTree)(implicit ctx: Context): TypeTree

  /** Type tree representing wildcard type bounds written in the source.
   *  The wildcard type `_` (for example in in `List[_]`) will be a type tree that
   *  represents a type but has `TypeBound`a inside.
   */
  type WildcardTypeTree <: TypeOrBoundsTree

  def isWildcardTypeTree(x: TypeOrBoundsTree)(implicit ctx: Context): Option[WildcardTypeTree]

  //
  // TYPES
  //

  /** Type or bounds */
  type TypeOrBounds <: AnyRef

  /** NoPrefix for a type selection */
  type NoPrefix <: TypeOrBounds

  def isNoPrefix(x: TypeOrBounds)(implicit ctx: Context): Option[NoPrefix]

  /** Type bounds */
  type TypeBounds <: TypeOrBounds

  def isTypeBounds(x: TypeOrBounds)(implicit ctx: Context): Option[TypeBounds]

  def TypeBounds_low(self: TypeBounds)(implicit ctx: Context): Type
  def TypeBounds_hi(self: TypeBounds)(implicit ctx: Context): Type

  /** A type */
  type Type <: TypeOrBounds

  def isType(x: TypeOrBounds)(implicit ctx: Context): Option[Type]

  def `Type_=:=`(self: Type)(that: Type)(implicit ctx: Context): Boolean
  def `Type_<:<`(self: Type)(that: Type)(implicit ctx: Context): Boolean

  /** Widen from singleton type to its underlying non-singleton
    *  base type by applying one or more `underlying` dereferences,
    *  Also go from => T to T.
    *  Identity for all other types. Example:
    *
    *  class Outer { class C ; val x: C }
    *  def o: Outer
    *  <o.x.type>.widen = o.C
    */
  def Type_widen(self: Type)(implicit ctx: Context): Type

  def Type_classSymbol(self: Type)(implicit ctx: Context): Option[ClassSymbol]

  def Type_typeSymbol(self: Type)(implicit ctx: Context): Symbol

  def Type_isSingleton(self: Type)(implicit ctx: Context): Boolean

  def Type_memberType(self: Type)(member: Symbol)(implicit ctx: Context): Type

  /** A singleton type representing a known constant value */
  type ConstantType <: Type

  def isConstantType(tpe: TypeOrBounds)(implicit ctx: Context): Option[ConstantType]

  def ConstantType_constant(self: ConstantType)(implicit ctx: Context): Constant

  /** Type of a reference to a symbol */
  type SymRef <: Type

  def isSymRef(tpe: TypeOrBounds)(implicit ctx: Context): Option[SymRef]

  // TODO remove this method. May require splitting SymRef into TypeSymRef and TermSymRef
  def isSymRef_unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[(Symbol, TypeOrBounds /* Type | NoPrefix */)]

  def SymRef_qualifier(self: SymRef)(implicit ctx: Context): TypeOrBounds

  /** Type of a reference to a term */
  type TermRef <: Type

  def isTermRef(tpe: TypeOrBounds)(implicit ctx: Context): Option[TermRef]

  def TermRef_name(self: TermRef)(implicit ctx: Context): String
  def TermRef_qualifier(self: TermRef)(implicit ctx: Context): TypeOrBounds

  def TermRef_apply(qual: TypeOrBounds, name: String)(implicit ctx: Context): TermRef

  /** Type of a reference to a type */
  type TypeRef <: Type

  def isTypeRef(tpe: TypeOrBounds)(implicit ctx: Context): Option[TypeRef]

  def TypeRef_name(self: TypeRef)(implicit ctx: Context): String
  def TypeRef_qualifier(self: TypeRef)(implicit ctx: Context): TypeOrBounds

  /** Type of a `super` refernce */
  type SuperType <: Type

  def isSuperType(tpe: TypeOrBounds)(implicit ctx: Context): Option[SuperType]

  def SuperType_thistpe(self: SuperType)(implicit ctx: Context): Type
  def SuperType_supertpe(self: SuperType)(implicit ctx: Context): Type

  /** A type with a type refinement `T { type U }` */
  type Refinement <: Type

  def isRefinement(tpe: TypeOrBounds)(implicit ctx: Context): Option[Refinement]

  def Refinement_parent(self: Refinement)(implicit ctx: Context): Type
  def Refinement_name(self: Refinement)(implicit ctx: Context): String
  def Refinement_info(self: Refinement)(implicit ctx: Context): TypeOrBounds

  /** A higher kinded type applied to some types `T[U]` */
  type AppliedType <: Type

  def isAppliedType(tpe: TypeOrBounds)(implicit ctx: Context): Option[AppliedType]

  def AppliedType_tycon(self: AppliedType)(implicit ctx: Context): Type
  def AppliedType_args(self: AppliedType)(implicit ctx: Context): List[TypeOrBounds]

  /** A type with an anottation `T @foo` */
  type AnnotatedType <: Type

  def isAnnotatedType(tpe: TypeOrBounds)(implicit ctx: Context): Option[AnnotatedType]

  def AnnotatedType_underlying(self: AnnotatedType)(implicit ctx: Context): Type
  def AnnotatedType_annot(self: AnnotatedType)(implicit ctx: Context): Term

  /** Intersection type `T & U` */
  type AndType <: Type

  def isAndType(tpe: TypeOrBounds)(implicit ctx: Context): Option[AndType]

  def AndType_left(self: AndType)(implicit ctx: Context): Type
  def AndType_right(self: AndType)(implicit ctx: Context): Type

  /** Union type `T | U` */
  type OrType <: Type

  def isOrType(tpe: TypeOrBounds)(implicit ctx: Context): Option[OrType]

  def OrType_left(self: OrType)(implicit ctx: Context): Type
  def OrType_right(self: OrType)(implicit ctx: Context): Type

  /** Type match `T match { case U => ... }` */
  type MatchType <: Type

  def isMatchType(tpe: TypeOrBounds)(implicit ctx: Context): Option[MatchType]

  def MatchType_bound(self: MatchType)(implicit ctx: Context): Type
  def MatchType_scrutinee(self: MatchType)(implicit ctx: Context): Type
  def MatchType_cases(self: MatchType)(implicit ctx: Context): List[Type]

  /** Type of a by by name parameter */
  type ByNameType <: Type

  def isByNameType(tpe: TypeOrBounds)(implicit ctx: Context): Option[ByNameType]

  def ByNameType_underlying(self: ByNameType)(implicit ctx: Context): Type

  /** Type of a parameter reference */
  type ParamRef <: Type

  def isParamRef(tpe: TypeOrBounds)(implicit ctx: Context): Option[ParamRef]

  def ParamRef_binder(self: ParamRef)(implicit ctx: Context): LambdaType[TypeOrBounds]
  def ParamRef_paramNum(self: ParamRef)(implicit ctx: Context): Int

  /** Type of `this` */
  type ThisType <: Type

  def isThisType(tpe: TypeOrBounds)(implicit ctx: Context): Option[ThisType]

  def ThisType_tref(self: ThisType)(implicit ctx: Context): Type

  /** A type that is recursively defined `this` */
  type RecursiveThis <: Type

  def isRecursiveThis(tpe: TypeOrBounds)(implicit ctx: Context): Option[RecursiveThis]

  def RecursiveThis_binder(self: RecursiveThis)(implicit ctx: Context): RecursiveType

  /** A type that is recursively defined */
  type RecursiveType <: Type

  def isRecursiveType(tpe: TypeOrBounds)(implicit ctx: Context): Option[RecursiveType]

  def RecursiveType_underlying(self: RecursiveType)(implicit ctx: Context): Type

  // TODO can we add the bound back without an cake?
  // TODO is LambdaType really needed? ParamRefExtractor could be split into more precise extractors
  /** Common abstraction for lambda types (MethodType, PolyType and TypeLambda). */
  type LambdaType[ParamInfo /*<: TypeOrBounds*/] <: Type

  /** Type of the definition of a method taking a single list of parameters. It's return type may be a MethodType. */
  type MethodType <: LambdaType[Type]

  def isMethodType(tpe: TypeOrBounds)(implicit ctx: Context): Option[MethodType]

  def MethodType_isErased(self: MethodType): Boolean
  def MethodType_isImplicit(self: MethodType): Boolean
  def MethodType_paramNames(self: MethodType)(implicit ctx: Context): List[String]
  def MethodType_paramTypes(self: MethodType)(implicit ctx: Context): List[Type]
  def MethodType_resType(self: MethodType)(implicit ctx: Context): Type

  /** Type of the definition of a method taking a list of type parameters. It's return type may be a MethodType. */
  type PolyType <: LambdaType[TypeBounds]

  def isPolyType(tpe: TypeOrBounds)(implicit ctx: Context): Option[PolyType]

  def PolyType_paramNames(self: PolyType)(implicit ctx: Context): List[String]
  def PolyType_paramBounds(self: PolyType)(implicit ctx: Context): List[TypeBounds]
  def PolyType_resType(self: PolyType)(implicit ctx: Context): Type

  /** Type of the definition of a type lambda taking a list of type parameters. It's return type may be a TypeLambda. */
  type TypeLambda <: LambdaType[TypeBounds]

  def isTypeLambda(tpe: TypeOrBounds)(implicit ctx: Context): Option[TypeLambda]

  def TypeLambda_paramNames(self: TypeLambda)(implicit ctx: Context): List[String]
  def TypeLambda_paramBounds(self: TypeLambda)(implicit ctx: Context): List[TypeBounds]
  def TypeLambda_resType(self: TypeLambda)(implicit ctx: Context): Type

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

  def isSimpleSelector(self: ImportSelector)(implicit ctx: Context): Option[SimpleSelector]

  def SimpleSelector_selection(self: SimpleSelector)(implicit ctx: Context): Id

  type RenameSelector <: ImportSelector

  def isRenameSelector(self: ImportSelector)(implicit ctx: Context): Option[RenameSelector]

  def RenameSelector_from(self: RenameSelector)(implicit ctx: Context): Id
  def RenameSelector_to(self: RenameSelector)(implicit ctx: Context): Id

  type OmitSelector <: ImportSelector

  def isOmitSelector(self: ImportSelector)(implicit ctx: Context): Option[OmitSelector]

  def SimpleSelector_omited(self: OmitSelector)(implicit ctx: Context): Id

  //
  // IDENTIFIERS
  //

  /** Untyped identifier */
  type Id <: AnyRef

  /** Position in the source code */
  def Id_pos(self: Id)(implicit ctx: Context): Position

  /** Name of the identifier */
  def Id_name(self: Id)(implicit ctx: Context): String

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

  /** Source position */
  type Position <: AnyRef

  /** The start offset in the source file */
  def Position_start(self: Position): Int

  /** The end offset in the source file */
  def Position_end(self: Position): Int

  /** Does this position exist */
  def Position_exists(self: Position): Boolean

  /** Source file in which this position is located */
  def Position_sourceFile(self: Position): java.nio.file.Path

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
  // COMMENTS
  //

  /** Comment */
  type Comment <: AnyRef

  //
  // CONSTANTS
  //

  /** Constant value represented as the constant itself */
  type Constant <: AnyRef

  def Constant_value(const: Constant): Any

  def isConstant_Unit(constant: Constant): Boolean
  def isConstant_Null(constant: Constant): Boolean
  def isConstant_Boolean(constant: Constant): Option[Boolean]
  def isConstant_Byte(constant: Constant): Option[Byte]
  def isConstant_Short(constant: Constant): Option[Short]
  def isConstant_Char(constant: Constant): Option[Char]
  def isConstant_Int(constant: Constant): Option[Int]
  def isConstant_Long(constant: Constant): Option[Long]
  def isConstant_Float(constant: Constant): Option[Float]
  def isConstant_Double(constant: Constant): Option[Double]
  def isConstant_String(constant: Constant): Option[String]
  def isConstant_ClassTag(constant: Constant): Option[Type]
  def isConstant_Symbol(constant: Constant): Option[scala.Symbol]

  def Constant_Unit_apply(): Constant
  def Constant_Null_apply(): Constant
  def Constant_Boolean_apply(x: Boolean): Constant
  def Constant_Byte_apply(x: Byte): Constant
  def Constant_Short_apply(x: Short): Constant
  def Constant_Char_apply(x: Char): Constant
  def Constant_Int_apply(x: Int): Constant
  def Constant_Long_apply(x: Long): Constant
  def Constant_Float_apply(x: Float): Constant
  def Constant_Double_apply(x: Double): Constant
  def Constant_String_apply(x: String): Constant
  def Constant_ClassTag_apply(x: scala.reflect.ClassTag[_]): Constant
  def Constant_Symbol_apply(x: scala.Symbol): Constant

  //
  // SYMBOLS
  //

  /** Symbol of a definition.
   *  Then can be compared with == to know if the definition is the same.
   */
  type Symbol <: AnyRef

  /** Owner of this symbol. The owner is the symbol in which this symbol is defined. */
  def Symbol_owner(self: Symbol)(implicit ctx: Context): Symbol

  /** Flags of this symbol */
  def Symbol_flags(self: Symbol)(implicit ctx: Context): Flags

  def Symbol_isLocalDummy(self: Symbol)(implicit ctx: Context): Boolean

  def Symbol_isRefinementClass(self: Symbol)(implicit ctx: Context): Boolean

  def Symbol_isAliasType(self: Symbol)(implicit ctx: Context): Boolean

  def Symbol_isAnonymousClass(self: Symbol)(implicit ctx: Context): Boolean

  def Symbol_isAnonymousFunction(self: Symbol)(implicit ctx: Context): Boolean

  def Symbol_isAbstractType(self: Symbol)(implicit ctx: Context): Boolean

  def Symbol_isClassConstructor(self: Symbol)(implicit ctx: Context): Boolean

  /** This symbol is private within the resulting type. */
  def Symbol_privateWithin(self: Symbol)(implicit ctx: Context): Option[Type]

  /** This symbol is protected within the resulting type. */
  def Symbol_protectedWithin(self: Symbol)(implicit ctx: Context): Option[Type]

  /** The name of this symbol. */
  def Symbol_name(self: Symbol)(implicit ctx: Context): String

  /** The full name of this symbol up to the root package. */
  def Symbol_fullName(self: Symbol)(implicit ctx: Context): String

  /** The position of this symbol */
  def Symbol_pos(self: Symbol)(implicit ctx: Context): Position

  def Symbol_localContext(self: Symbol)(implicit ctx: Context): Context

  /** The comment of the symbol */
  def Symbol_comment(self: Symbol)(implicit ctx: Context): Option[Comment]

  /** Annotations attached to this symbol */
  def Symbol_annots(self: Symbol)(implicit ctx: Context): List[Term]

  def Symbol_isDefinedInCurrentRun(self: Symbol)(implicit ctx: Context): Boolean

  /** Symbol of a package definition */
  type PackageSymbol <: Symbol

  def isPackageSymbol(symbol: Symbol)(implicit ctx: Context): Option[PackageSymbol]

  /** Symbol of a class definition. This includes anonymous class definitions and the class of a module object. */
  type ClassSymbol <: Symbol

  def isClassSymbol(symbol: Symbol)(implicit ctx: Context): Option[ClassSymbol]

  /** Symbol of a type (parameter or member) definition. */
  type TypeSymbol <: Symbol

  def isTypeSymbol(symbol: Symbol)(implicit ctx: Context): Option[TypeSymbol]

  /** Symbol representing a method definition. */
  type DefSymbol <: Symbol

  def isDefSymbol(symbol: Symbol)(implicit ctx: Context): Option[DefSymbol]

  /** Symbol representing a value definition. This includes `val`, `lazy val`, `var`, `object` and parameter definitions. */
  type ValSymbol <: Symbol

  def isValSymbol(symbol: Symbol)(implicit ctx: Context): Option[ValSymbol]

  /** Symbol representing a bind definition. */
  type BindSymbol <: Symbol

  def isBindSymbol(symbol: Symbol)(implicit ctx: Context): Option[BindSymbol]

  /** No symbol available. */
  type NoSymbol <: Symbol

  def isNoSymbol(symbol: Symbol)(implicit ctx: Context): Boolean

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

  def Flags_Private: Flags
  def Flags_Protected: Flags
  def Flags_Abstract: Flags
  def Flags_Final: Flags
  def Flags_Sealed: Flags
  def Flags_Case: Flags
  def Flags_Implicit: Flags
  def Flags_Implied: Flags
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
  def Flags_ImplClass: Flags

  //
  // QUOTED SEAL/UNSEAL
  //

  /** View this expression `Expr[_]` as a `Term` */
  def QuotedExpr_unseal(self: scala.quoted.Expr[_])(implicit ctx: Context): Term

  /** View this expression `Type[T]` as a `TypeTree` */
  def QuotedType_unseal(self: scala.quoted.Type[_])(implicit ctx: Context): TypeTree

  /** Convert `Term` to an `Expr[T]` and check that it conforms to `T` */
  def QuotedExpr_seal[T](self: Term)(tpe: scala.quoted.Type[T])(implicit ctx: Context): scala.quoted.Expr[T]

  /** Convert `Type` to an `quoted.Type[T]` */
  def QuotedType_seal(self: Type)(implicit ctx: Context): scala.quoted.Type[_]

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

  def Definitions_JavaLangPackage: Symbol

  def Definitions_ArrayModule: Symbol

  def Definitions_Array_apply: Symbol
  def Definitions_Array_clone: Symbol
  def Definitions_Array_length: Symbol
  def Definitions_Array_update: Symbol

  def Definitions_RepeatedParamClass: Symbol

  def Definitions_OptionClass: Symbol
  def Definitions_NoneModule: Symbol
  def Definitions_SomeModule: Symbol

  def Definitions_ProductClass: Symbol
  // TODO avoid default parameters
  def Definitions_FunctionClass(arity: Int, isImplicit: Boolean = false, isErased: Boolean = false): Symbol

  def Definitions_TupleClass(arity: Int): Symbol

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

}
