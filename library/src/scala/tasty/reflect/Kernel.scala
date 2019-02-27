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

  def PackageClause_pid(self: PackageClause)(implicit ctx: Context): Term_Ref
  def PackageClause_stats(self: PackageClause)(implicit ctx: Context): List[Tree]

  /** Tree representing a statement in the source code */
  type Statement <: Tree

  /** Tree representing an import in the source code */
  type Import <: Statement

  def Import_impliedOnly(self: Import): Boolean
  def Import_expr(self: Import)(implicit ctx: Context): Term
  def Import_selectors(self: Import)(implicit ctx: Context): List[ImportSelector]

  /** Tree representing a definition in the source code. It can be `PackageDef`, `ClassDef`, `TypeDef`, `DefDef` or `ValDef` */
  type Definition <: Statement

  def Definition_name(self: Definition)(implicit ctx: Context): String

  /** Tree representing a package definition. This includes definitions in all source files */
  type PackageDef <: Definition

  def PackageDef_owner(self: PackageDef)(implicit ctx: Context): PackageDef
  def PackageDef_members(self: PackageDef)(implicit ctx: Context): List[Statement]
  def PackageDef_symbol(self: PackageDef)(implicit ctx: Context): PackageSymbol

  /** Tree representing a class definition. This includes annonymus class definitions and the class of a module object */
  type ClassDef <: Definition

  def ClassDef_constructor(self: ClassDef)(implicit ctx: Context): DefDef
  def ClassDef_parents(self: ClassDef)(implicit ctx: Context): List[TermOrTypeTree]
  def ClassDef_derived(self: ClassDef)(implicit ctx: Context): List[TypeTree]
  def ClassDef_self(self: ClassDef)(implicit ctx: Context): Option[ValDef]
  def ClassDef_body(self: ClassDef)(implicit ctx: Context): List[Statement]
  def ClassDef_symbol(self: ClassDef)(implicit ctx: Context): ClassSymbol

  /** Tree representing a type (paramter or member) definition in the source code */
  type TypeDef <: Definition

  def TypeDef_rhs(self: TypeDef)(implicit ctx: Context): TypeOrBoundsTree
  def TypeDef_symbol(self: TypeDef)(implicit ctx: Context): TypeSymbol

  /** Tree representing a method definition in the source code */
  type DefDef <: Definition

  def DefDef_typeParams(self: DefDef)(implicit ctx: Context): List[TypeDef]
  def DefDef_paramss(self: DefDef)(implicit ctx: Context): List[List[ValDef]]
  def DefDef_returnTpt(self: DefDef)(implicit ctx: Context): TypeTree
  def DefDef_rhs(self: DefDef)(implicit ctx: Context): Option[Term]
  def DefDef_symbol(self: DefDef)(implicit ctx: Context): DefSymbol

  /** Tree representing a value definition in the source code This inclues `val`, `lazy val`, `var`, `object` and parameter defintions. */
  type ValDef <: Definition

  def ValDef_tpt(self: ValDef)(implicit ctx: Context): TypeTree
  def ValDef_rhs(self: ValDef)(implicit ctx: Context): Option[Term]
  def ValDef_symbol(self: ValDef)(implicit ctx: Context): ValSymbol

  /** Tree representing an expression in the source code */
  type Term <: Statement

  def Term_pos(self: Term)(implicit ctx: Context): Position
  def Term_tpe(self: Term)(implicit ctx: Context): Type
  def Term_underlyingArgument(self: Term)(implicit ctx: Context): Term
  def Term_underlying(self: Term)(implicit ctx: Context): Term

  /** Tree representing a reference to definition */
  type Term_Ref <: Term

  /** Tree representing a reference to definition with a given name */
  type Term_Ident <: Term_Ref

  def Term_Ident_name(self: Term_Ident)(implicit ctx: Context): String

  /** Tree representing a selection of definition with a given name on a given prefix */
  type Term_Select <: Term_Ref

  def Term_Select_qualifier(self: Term_Select)(implicit ctx: Context): Term
  def Term_Select_name(self: Term_Select)(implicit ctx: Context): String
  def Term_Select_signature(self: Term_Select)(implicit ctx: Context): Option[Signature]

  /** Tree representing a literal value in the source code */
  type Term_Literal <: Term

  def Term_Literal_constant(self: Term_Literal)(implicit ctx: Context): Constant

  /** Tree representing `this` in the source code */
  type Term_This <: Term

  def Term_This_id(self: Term_This)(implicit ctx: Context): Option[Id]

  /** Tree representing `new` in the source code */
  type Term_New <: Term

  def Term_New_tpt(self: Term_New)(implicit ctx: Context): TypeTree

  /** Tree representing an argument passed with an explicit name. Such as `arg1 = x` in `foo(arg1 = x)` */
  type Term_NamedArg <: Term

  def Term_NamedArg_name(self: Term_NamedArg)(implicit ctx: Context): String
  def Term_NamedArg_value(self: Term_NamedArg)(implicit ctx: Context): Term

  /** Tree an application of arguments. It represents a single list of arguments, multiple argument lists will have nested `Apply`s */
  type Term_Apply <: Term

  def Term_Apply_fun(self: Term_Apply)(implicit ctx: Context): Term
  def Term_Apply_args(self: Term_Apply)(implicit ctx: Context): List[Term]

  /** Tree an application of type arguments */
  type Term_TypeApply <: Term

  def Term_TypeApply_fun(self: Term_TypeApply)(implicit ctx: Context): Term
  def Term_TypeApply_args(self: Term_TypeApply)(implicit ctx: Context): List[TypeTree]

  /** Tree representing `super` in the source code */
  type Term_Super <: Term

  def Term_Super_qualifier(self: Term_Super)(implicit ctx: Context): Term
  def Term_Super_id(self: Term_Super)(implicit ctx: Context): Option[Id]

  /** Tree representing a type ascription `x: T` in the source code */
  type Term_Typed <: Term

  def Term_Typed_expr(self: Term_Typed)(implicit ctx: Context): Term
  def Term_Typed_tpt(self: Term_Typed)(implicit ctx: Context): TypeTree

  /** Tree representing an assignment `x = y` in the source code */
  type Term_Assign <: Term

  def Term_Assign_lhs(self: Term_Assign)(implicit ctx: Context): Term
  def Term_Assign_rhs(self: Term_Assign)(implicit ctx: Context): Term

  /** Tree representing a block `{ ... }` in the source code */
  type Term_Block <: Term

  def Term_Block_statements(self: Term_Block)(implicit ctx: Context): List[Statement]
  def Term_Block_expr(self: Term_Block)(implicit ctx: Context): Term

  /** Tree representing a lambda `(...) => ...` in the source code */
  type Term_Lambda <: Term

  def Term_Lambda_meth(self: Term_Lambda)(implicit ctx: Context): Term
  def Term_Lambda_tptOpt(self: Term_Lambda)(implicit ctx: Context): Option[TypeTree]

  /** Tree representing an if/then/else `if (...) ... else ...` in the source code */
  type Term_If <: Term

  def Term_If_cond(self: Term_If)(implicit ctx: Context): Term
  def Term_If_thenp(self: Term_If)(implicit ctx: Context): Term
  def Term_If_elsep(self: Term_If)(implicit ctx: Context): Term

  /** Tree representing a pattern match `x match  { ... }` in the source code */
  type Term_Match <: Term

  def Term_Match_scrutinee(self: Term_Match)(implicit ctx: Context): Term
  def Term_Match_cases(self: Term_Match)(implicit ctx: Context): List[CaseDef]

  /** Tree representing a tyr catch `try x catch { ... } finally { ... }` in the source code */
  type Term_Try <: Term

  def Term_Try_body(self: Term_Try)(implicit ctx: Context): Term
  def Term_Try_cases(self: Term_Try)(implicit ctx: Context): List[CaseDef]
  def Term_Try_finalizer(self: Term_Try)(implicit ctx: Context): Option[Term]

  /** Tree representing a `return` in the source code */
  type Term_Return <: Term

  def Term_Return_expr(self: Term_Return)(implicit ctx: Context): Term

  /** Tree representing a variable argument list in the source code */
  type Term_Repeated <: Term

  def Term_Repeated_elems(self: Term_Repeated)(implicit ctx: Context): List[Term]
  def Term_Repeated_elemtpt(self: Term_Repeated)(implicit ctx: Context): TypeTree

  /** Tree representing the scope of an inlined tree */
  type Term_Inlined <: Term

  def Term_Inlined_call(self: Term_Inlined)(implicit ctx: Context): Option[TermOrTypeTree]
  def Term_Inlined_bindings(self: Term_Inlined)(implicit ctx: Context): List[Definition]
  def Term_Inlined_body(self: Term_Inlined)(implicit ctx: Context): Term

  /** Tree representing a selection of definition with a given name on a given prefix and number of nested scopes of inlined trees */
  type Term_SelectOuter <: Term

  def Term_SelectOuter_qualifier(self: Term_SelectOuter)(implicit ctx: Context): Term
  def Term_SelectOuter_level(self: Term_SelectOuter)(implicit ctx: Context): Int
  def Term_SelectOuter_tpe(self: Term_SelectOuter)(implicit ctx: Context): Type

  /** Tree representing a while loop */
  type Term_While <: Term

  def Term_While_cond(self: Term_While)(implicit ctx: Context): Term
  def Term_While_body(self: Term_While)(implicit ctx: Context): Term

  //
  // CASES
  //
  
  /** Branch of a pattern match or catch clause */
  type CaseDef <: AnyRef

  def CaseDef_pattern(self: CaseDef)(implicit ctx: Context): Pattern
  def CaseDef_guard(self: CaseDef)(implicit ctx: Context): Option[Term]
  def CaseDef_rhs(self: CaseDef)(implicit ctx: Context): Term

  /** Branch of a type pattern match */
  type TypeCaseDef <: AnyRef

  def TypeCaseDef_pattern(self: TypeCaseDef)(implicit ctx: Context): TypeTree
  def TypeCaseDef_rhs(self: TypeCaseDef)(implicit ctx: Context): TypeTree

  //
  // PATTERNS
  //
  
  /** Pattern tree of the pattern part of a CaseDef */
  type Pattern <: AnyRef

  /** Pattern representing a value. This includes `1`, ```x``` and `_` */
  type Value <: Pattern

  def Pattern_Value_value(self: Value)(implicit ctx: Context): Term

  /** Pattern representing a `_ @ _` binding. */
  type Bind <: Pattern

  def Pattern_Bind_name(self: Bind)(implicit ctx: Context): String

  def Pattern_Bind_pattern(self: Bind)(implicit ctx: Context): Pattern

  /** Pattern representing a `Xyz(...)` unapply. */
  type Unapply <: Pattern

  def Pattern_Unapply_fun(self: Unapply)(implicit ctx: Context): Term

  def Pattern_Unapply_implicits(self: Unapply)(implicit ctx: Context): List[Term]

  def Pattern_Unapply_patterns(self: Unapply)(implicit ctx: Context): List[Pattern]

  /** Pattern representing `X | Y | ...` alternatives. */
  type Alternatives <: Pattern

  def Pattern_Alternatives_patterns(self: Alternatives)(implicit ctx: Context): List[Pattern]

  /** Pattern representing a `x: Y` type test. */
  type TypeTest <: Pattern

  def Pattern_TypeTest_tpt(self: TypeTest)(implicit ctx: Context): TypeTree

  //
  // TYPE TREES
  //
  
  /** Type tree representing a type or a bounds written in the source */
  type TypeOrBoundsTree <: AnyRef

  def TypeOrBoundsTree_tpe(self: TypeOrBoundsTree)(implicit ctx: Context): Type

  /** Type tree representing a type written in the source */
  type TypeTree <: TypeOrBoundsTree

  def TypeTree_pos(self: TypeTree)(implicit ctx: Context): Position
  def TypeTree_symbol(self: TypeTree)(implicit ctx: Context): Symbol
  def TypeTree_tpe(self: TypeTree)(implicit ctx: Context): Type

  /** Type tree representing an inferred type */
  type TypeTree_Inferred <: TypeTree

  /** Type tree representing a reference to definition with a given name */
  type TypeTree_Ident <: TypeTree

  def TypeTree_Ident_name(self: TypeTree_Ident)(implicit ctx: Context): String

  /** Type tree representing a selection of definition with a given name on a given term prefix */
  type TypeTree_Select <: TypeTree

  def TypeTree_Select_qualifier(self: TypeTree_Select)(implicit ctx: Context): Term
  def TypeTree_Select_name(self: TypeTree_Select)(implicit ctx: Context): String

  /** Type tree representing a selection of definition with a given name on a given type prefix */
  type TypeTree_Projection <: TypeTree

  def TypeTree_Projection_qualifier(self: TypeTree_Projection)(implicit ctx: Context): TypeTree
  def TypeTree_Projection_name(self: TypeTree_Projection)(implicit ctx: Context): String

  /** Type tree representing a singleton type */
  type TypeTree_Singleton <: TypeTree

  def TypeTree_Singleton_ref(self: TypeTree_Singleton)(implicit ctx: Context): Term

  /** Type tree representing a type refinement */
  type TypeTree_Refined <: TypeTree

  def TypeTree_Refined_tpt(self: TypeTree_Refined)(implicit ctx: Context): TypeTree
  def TypeTree_Refined_refinements(self: TypeTree_Refined)(implicit ctx: Context): List[Definition]

  /** Type tree representing a type application */
  type TypeTree_Applied <: TypeTree

  def TypeTree_Applied_tpt(self: TypeTree_Applied)(implicit ctx: Context): TypeTree
  def TypeTree_Applied_args(self: TypeTree_Applied)(implicit ctx: Context): List[TypeOrBoundsTree]

  /** Type tree representing an annotated type */
  type TypeTree_Annotated <: TypeTree

  def TypeTree_Annotated_arg(self: TypeTree_Annotated)(implicit ctx: Context): TypeTree
  def TypeTree_Annotated_annotation(self: TypeTree_Annotated)(implicit ctx: Context): Term

  /** Type tree representing a type match */
  type TypeTree_MatchType <: TypeTree

  def TypeTree_MatchType_bound(self: TypeTree_MatchType)(implicit ctx: Context): Option[TypeTree]
  def TypeTree_MatchType_selector(self: TypeTree_MatchType)(implicit ctx: Context): TypeTree
  def TypeTree_MatchType_cases(self: TypeTree_MatchType)(implicit ctx: Context): List[TypeCaseDef]

  /** Type tree representing a by name parameter */
  type TypeTree_ByName <: TypeTree

  def TypeTree_ByName_result(self: TypeTree_ByName)(implicit ctx: Context): TypeTree

  /** Type tree representing a lambda abstraction type */
  type TypeTree_LambdaTypeTree <: TypeTree

  def TypeTree_LambdaTypeTree_tparams(self: TypeTree_LambdaTypeTree)(implicit ctx: Context): List[TypeDef]
  def TypeTree_LambdaTypeTree_body(self: TypeTree_LambdaTypeTree)(implicit ctx: Context): TypeOrBoundsTree

  /** Type tree representing a type binding */
  type TypeTree_TypeBind <: TypeTree

  def TypeTree_TypeBind_name(self: TypeTree_TypeBind)(implicit ctx: Context): String
  def TypeTree_TypeBind_body(self: TypeTree_TypeBind)(implicit ctx: Context): TypeOrBoundsTree

  /** Type tree within a block with aliases `{ type U1 = ... ; T[U1, U2] }` */
  type TypeTree_TypeBlock <: TypeTree

  def TypeTree_TypeBlock_aliases(self: TypeTree_TypeBlock)(implicit ctx: Context): List[TypeDef]
  def TypeTree_TypeBlock_tpt(self: TypeTree_TypeBlock)(implicit ctx: Context): TypeTree

  /** Type tree representing a type bound written in the source */
  type TypeBoundsTree <: TypeOrBoundsTree

  def TypeBoundsTree_tpe(self: TypeBoundsTree)(implicit ctx: Context): TypeBounds
  def TypeBoundsTree_low(self: TypeBoundsTree)(implicit ctx: Context): TypeTree
  def TypeBoundsTree_hi(self: TypeBoundsTree)(implicit ctx: Context): TypeTree

  /** Type tree representing wildcard type bounds written in the source.
   *  The wildcard type `_` (for example in in `List[_]`) will be a type tree that
   *  represents a type but has `TypeBound`a inside.
   */
  type WildcardType <: TypeOrBoundsTree

  //
  // TYPES
  //

  /** Type or bounds */
  type TypeOrBounds <: AnyRef

  /** NoPrefix for a type selection */
  type NoPrefix <: TypeOrBounds

  /** Type bounds */
  type TypeBounds <: TypeOrBounds

  def TypeBounds_low(self: TypeBounds)(implicit ctx: Context): Type
  def TypeBounds_hi(self: TypeBounds)(implicit ctx: Context): Type

  /** A type */
  type Type <: TypeOrBounds

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

  def ConstantType_value(self: ConstantType)(implicit ctx: Context): Any

  /** Type of a reference to a symbol */
  type SymRef <: Type

  def SymRef_qualifier(self: SymRef)(implicit ctx: Context): TypeOrBounds

  /** Type of a reference to a term */
  type TermRef <: Type

  def TermRef_qualifier(self: TermRef)(implicit ctx: Context): TypeOrBounds

  /** Type of a reference to a type */
  type TypeRef <: Type

  def TypeRef_name(self: TypeRef)(implicit ctx: Context): String
  def TypeRef_qualifier(self: TypeRef)(implicit ctx: Context): TypeOrBounds

  /** Type of a `super` refernce */
  type SuperType <: Type

  def SuperType_thistpe(self: SuperType)(implicit ctx: Context): Type
  def SuperType_supertpe(self: SuperType)(implicit ctx: Context): Type

  /** A type with a type refinement `T { type U }` */
  type Refinement <: Type

  def Refinement_parent(self: Refinement)(implicit ctx: Context): Type
  def Refinement_name(self: Refinement)(implicit ctx: Context): String
  def Refinement_info(self: Refinement)(implicit ctx: Context): TypeOrBounds

  /** A higher kinded type applied to some types `T[U]` */
  type AppliedType <: Type

  def AppliedType_tycon(self: AppliedType)(implicit ctx: Context): Type
  def AppliedType_args(self: AppliedType)(implicit ctx: Context): List[TypeOrBounds]

  /** A type with an anottation `T @foo` */
  type AnnotatedType <: Type

  def AnnotatedType_underlying(self: AnnotatedType)(implicit ctx: Context): Type
  def AnnotatedType_annot(self: AnnotatedType)(implicit ctx: Context): Term

  /** Intersection type `T & U` */
  type AndType <: Type

  def AndType_left(self: AndType)(implicit ctx: Context): Type
  def AndType_right(self: AndType)(implicit ctx: Context): Type

  /** Union type `T | U` */
  type OrType <: Type

  def OrType_left(self: OrType)(implicit ctx: Context): Type
  def OrType_right(self: OrType)(implicit ctx: Context): Type

  /** Type match `T match { case U => ... }` */
  type MatchType <: Type

  def MatchType_bound(self: MatchType)(implicit ctx: Context): Type
  def MatchType_scrutinee(self: MatchType)(implicit ctx: Context): Type
  def MatchType_cases(self: MatchType)(implicit ctx: Context): List[Type]

  /** Type of a by by name parameter */
  type ByNameType <: Type

  def ByNameType_underlying(self: ByNameType)(implicit ctx: Context): Type

  /** Type of a parameter reference */
  type ParamRef <: Type

  def ParamRef_binder(self: ParamRef)(implicit ctx: Context): LambdaType[TypeOrBounds]
  def ParamRef_paramNum(self: ParamRef)(implicit ctx: Context): Int

  /** Type of `this` */
  type ThisType <: Type

  def ThisType_underlying(self: ThisType)(implicit ctx: Context): Type

  /** A type that is recursively defined `this` */
  type RecursiveThis <: Type

  def RecursiveThis_binder(self: RecursiveThis)(implicit ctx: Context): RecursiveType

  /** A type that is recursively defined */
  type RecursiveType <: Type

  def RecursiveType_underlying(self: RecursiveType)(implicit ctx: Context): Type

  // TODO can we add the bound back without an cake?
  // TODO is LambdaType really needed? ParamRefExtractor could be split into more precise extractors
  /** Common abstraction for lambda types (MethodType, PolyType and TypeLambda). */
  type LambdaType[ParamInfo /*<: TypeOrBounds*/] <: Type

  /** Type of the definition of a method taking a single list of parameters. It's return type may be a MethodType. */
  type MethodType <: LambdaType[Type]

  def MethodType_isErased(self: MethodType): Boolean
  def MethodType_isImplicit(self: MethodType): Boolean
  def MethodType_paramNames(self: MethodType)(implicit ctx: Context): List[String]
  def MethodType_paramTypes(self: MethodType)(implicit ctx: Context): List[Type]
  def MethodType_resType(self: MethodType)(implicit ctx: Context): Type

  /** Type of the definition of a method taking a list of type parameters. It's return type may be a MethodType. */
  type PolyType <: LambdaType[TypeBounds]

  def PolyType_paramNames(self: PolyType)(implicit ctx: Context): List[String]
  def PolyType_paramBounds(self: PolyType)(implicit ctx: Context): List[TypeBounds]
  def PolyType_resType(self: PolyType)(implicit ctx: Context): Type

  /** Type of the definition of a type lambda taking a list of type parameters. It's return type may be a TypeLambda. */
  type TypeLambda <: LambdaType[TypeBounds]

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

  /** Symbol of a class definition. This includes anonymous class definitions and the class of a module object. */
  type ClassSymbol <: Symbol

  /** Symbol of a type (parameter or member) definition. */
  type TypeSymbol <: Symbol

  /** Symbol representing a method definition. */
  type DefSymbol <: Symbol

  /** Symbol representing a value definition. This includes `val`, `lazy val`, `var`, `object` and parameter definitions. */
  type ValSymbol <: Symbol

  /** Symbol representing a bind definition. */
  type BindSymbol <: Symbol

  /** No symbol available. */
  type NoSymbol <: Symbol

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

}
