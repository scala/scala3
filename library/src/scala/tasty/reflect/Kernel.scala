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

  /** Tree representing a pacakage clause in the source code */
  type PackageClause <: Tree

  /** Tree representing a statement in the source code */
  type Statement <: Tree

  /** Tree representing an import in the source code */
  type Import <: Statement

  /** Tree representing a definition in the source code. It can be `PackageDef`, `ClassDef`, `TypeDef`, `DefDef` or `ValDef` */
  type Definition <: Statement

  /** Tree representing a package definition. This includes definitions in all source files */
  type PackageDef <: Definition

  /** Tree representing a class definition. This includes annonymus class definitions and the class of a module object */
  type ClassDef <: Definition

  /** Tree representing a type (paramter or member) definition in the source code */
  type TypeDef <: Definition

  /** Tree representing a method definition in the source code */
  type DefDef <: Definition

  /** Tree representing a value definition in the source code This inclues `val`, `lazy val`, `var`, `object` and parameter defintions. */
  type ValDef <: Definition

  /** Tree representing an expression in the source code */
  type Term <: Statement

  /** Tree representing a reference to definition */
  type Term_Ref <: Term

  /** Tree representing a reference to definition with a given name */
  type Term_Ident <: Term_Ref

  /** Tree representing a selection of definition with a given name on a given prefix */
  type Term_Select <: Term_Ref

  /** Tree representing a literal value in the source code */
  type Term_Literal <: Term

  /** Tree representing `this` in the source code */
  type Term_This <: Term

  /** Tree representing `new` in the source code */
  type Term_New <: Term

  /** Tree representing an argument passed with an explicit name. Such as `arg1 = x` in `foo(arg1 = x)` */
  type Term_NamedArg <: Term

  /** Tree an application of arguments. It represents a single list of arguments, multiple argument lists will have nested `Apply`s */
  type Term_Apply <: Term

  /** Tree an application of type arguments */
  type Term_TypeApply <: Term

  /** Tree representing `super` in the source code */
  type Term_Super <: Term

  /** Tree representing a type ascription `x: T` in the source code */
  type Term_Typed <: Term

  /** Tree representing an assignment `x = y` in the source code */
  type Term_Assign <: Term

  /** Tree representing a block `{ ... }` in the source code */
  type Term_Block <: Term

  /** Tree representing a lambda `(...) => ...` in the source code */
  type Term_Lambda <: Term

  /** Tree representing an if/then/else `if (...) ... else ...` in the source code */
  type Term_If <: Term

  /** Tree representing a pattern match `x match  { ... }` in the source code */
  type Term_Match <: Term

  /** Tree representing a tyr catch `try x catch { ... } finally { ... }` in the source code */
  type Term_Try <: Term

  /** Tree representing a `return` in the source code */
  type Term_Return <: Term

  /** Tree representing a variable argument list in the source code */
  type Term_Repeated <: Term

  /** Tree representing the scope of an inlined tree */
  type Term_Inlined <: Term

  /** Tree representing a selection of definition with a given name on a given prefix and number of nested scopes of inlined trees */
  type Term_SelectOuter <: Term

  /** Tree representing a while loop */
  type Term_While <: Term

  //
  // CASES
  //
  
  /** Branch of a pattern match or catch clause */
  type CaseDef <: AnyRef

  /** Branch of a type pattern match */
  type TypeCaseDef <: AnyRef

  //
  // PATTERNS
  //
  
  /** Pattern tree of the pattern part of a CaseDef */
  type Pattern <: AnyRef

  /** Pattern representing a value. This includes `1`, ```x``` and `_` */
  type Value <: Pattern

  /** Pattern representing a `_ @ _` binding. */
  type Bind <: Pattern

  /** Pattern representing a `Xyz(...)` unapply. */
  type Unapply <: Pattern

  /** Pattern representing `X | Y | ...` alternatives. */
  type Alternatives <: Pattern

  /** Pattern representing a `x: Y` type test. */
  type TypeTest <: Pattern

  //
  // TYPE TREES
  //
  
  /** Type tree representing a type or a bounds written in the source */
  type TypeOrBoundsTree <: AnyRef

  /** Type tree representing a type written in the source */
  type TypeTree <: TypeOrBoundsTree

  /** Type tree representing an inferred type */
  type TypeTree_Inferred <: TypeTree

  /** Type tree representing a reference to definition with a given name */
  type TypeTree_Ident <: TypeTree

  /** Type tree representing a selection of definition with a given name on a given term prefix */
  type TypeTree_Select <: TypeTree

  /** Type tree representing a selection of definition with a given name on a given type prefix */
  type TypeTree_Projection <: TypeTree

  /** Type tree representing a singleton type */
  type TypeTree_Singleton <: TypeTree

  /** Type tree representing a type refinement */
  type TypeTree_Refined <: TypeTree

  /** Type tree representing a type application */
  type TypeTree_Applied <: TypeTree

  /** Type tree representing an annotated type */
  type TypeTree_Annotated <: TypeTree

  /** Type tree representing a type match */
  type TypeTree_MatchType <: TypeTree

  /** Type tree representing a by name parameter */
  type TypeTree_ByName <: TypeTree

  /** Type tree representing a lambda abstraction type */
  type TypeTree_LambdaTypeTree <: TypeTree

  /** Type tree representing a type binding */
  type TypeTree_TypeBind <: TypeTree

  /** Type tree within a block with aliases `{ type U1 = ... ; T[U1, U2] }` */
  type TypeTree_TypeBlock <: TypeTree

  /** Type tree representing a type bound written in the source */
  type TypeBoundsTree <: TypeOrBoundsTree

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

  /** A type */
  type Type <: TypeOrBounds

  /** A singleton type representing a known constant value */
  type ConstantType <: Type

  /** Type of a reference to a symbol */
  type SymRef <: Type

  /** Type of a reference to a term */
  type TermRef <: Type

  /** Type of a reference to a type */
  type TypeRef <: Type

  /** Type of a `super` refernce */
  type SuperType <: Type

  /** A type with a type refinement `T { type U }` */
  type Refinement <: Type

  /** A higher kinded type applied to some types `T[U]` */
  type AppliedType <: Type

  /** A type with an anottation `T @foo` */
  type AnnotatedType <: Type

  /** Intersection type `T & U` */
  type AndType <: Type

  /** Union type `T | U` */
  type OrType <: Type

  /** Type match `T match { case U => ... }` */
  type MatchType <: Type

  /** Type of a by by name parameter */
  type ByNameType <: Type

  /** Type of a parameter reference */
  type ParamRef <: Type

  /** Type of `this` */
  type ThisType <: Type

  /** A type that is recursively defined `this` */
  type RecursiveThis <: Type

  /** A type that is recursively defined */
  type RecursiveType <: Type

  // TODO can we add the bound back without an cake?
  // TODO is LambdaType really needed? ParamRefExtractor could be split into more precise extractors
  /** Common abstraction for lambda types (MethodType, PolyType and TypeLambda). */
  type LambdaType[ParamInfo /*<: TypeOrBounds*/] <: Type

  /** Type of the definition of a method taking a single list of parameters. It's return type may be a MethodType. */
  type MethodType <: LambdaType[Type]

  /** Type of the definition of a method taking a list of type parameters. It's return type may be a MethodType. */
  type PolyType <: LambdaType[TypeBounds]

  /** Type of the definition of a type lambda taking a list of type parameters. It's return type may be a TypeLambda. */
  type TypeLambda <: LambdaType[TypeBounds]

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

  //
  // SIGNATURES
  //

  /** JVM signature of a method */
  type Signature <: AnyRef

  //
  // POSITIONS
  //

  /** Source position */
  type Position <: AnyRef

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

  //
  // SYMBOLS
  //

  /** Symbol of a definition.
   *  Then can be compared with == to know if the definition is the same.
   */
  type Symbol <: AnyRef

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

}
