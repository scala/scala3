package scala.tasty.reflect

import scala.internal.tasty.CompilerInterface

import scala.quoted.QuoteContext
import scala.quoted.show.SyntaxHighlight
import scala.tasty.reflect._

/** TASTy Reflect
 *
 *
 *  Type hierarchy
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
 *           |
 *           +- CaseDef
 *           |
 *           +- TypeCaseDef
 *           +- Bind
 *           +- Unapply
 *           +- Alternatives
 *
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
trait Types {

  /** Compilation context */
  type Context <: AnyRef

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

  /** Tree representing a type (parameter or member) definition in the source code */
  type TypeDef <: Definition

  /** Tree representing a method definition in the source code */
  type DefDef <: Definition

  /** Tree representing a value definition in the source code This inclues `val`, `lazy val`, `var`, `object` and parameter defintions. */
  type ValDef <: Definition

  /** Tree representing an expression in the source code */
  type Term <: Statement

  /** Tree representing a reference to definition */
  type Ref <: Term

  /** Tree representing a reference to definition with a given name */
  type Ident <: Ref

  /** Tree representing a selection of definition with a given name on a given prefix */
  type Select <: Ref

  /** Tree representing a literal value in the source code */
  type Literal <: Term

  /** Tree representing `this` in the source code */
  type This <: Term

  /** Tree representing `new` in the source code */
  type New <: Term

  /** Tree representing an argument passed with an explicit name. Such as `arg1 = x` in `foo(arg1 = x)` */
  type NamedArg <: Term

  /** Tree an application of arguments. It represents a single list of arguments, multiple argument lists will have nested `Apply`s  */
  type Apply <: Term

  /** Tree an application of type arguments */
  type TypeApply <: Term

  /** Tree representing `super` in the source code */
  type Super <: Term

  /** Tree representing a type ascription `x: T` in the source code */
  type Typed <: Term

  /** Tree representing an assignment `x = y` in the source code */
  type Assign <: Term

  /** Tree representing a block `{ ... }` in the source code */
  type Block <: Term

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

  /** Tree representing an if/then/else `if (...) ... else ...` in the source code */
  type If <: Term

  /** Tree representing a pattern match `x match  { ... }` in the source code */
  type Match <: Term

  /** Tree representing a pattern match `given match { ... }` in the source code */  // TODO: drop
  type GivenMatch <: Term

  /** Tree representing a try catch `try x catch { ... } finally { ... }` in the source code */
  type Try <: Term

  /** Tree representing a `return` in the source code */
  type Return <: Term

  /** Tree representing a variable argument list in the source code */
  type Repeated <: Term

  /** Tree representing the scope of an inlined tree */
  type Inlined <: Term

  /** Tree representing a selection of definition with a given name on a given prefix and number of nested scopes of inlined trees */
  type SelectOuter <: Term

  /** Tree representing a while loop */
  type While <: Term

  /** Type tree representing a type written in the source */
  type TypeTree <: Tree

  /** Type tree representing an inferred type */
  type Inferred <: TypeTree

  /** Type tree representing a reference to definition with a given name */
  type TypeIdent <: TypeTree

  /** Type tree representing a selection of definition with a given name on a given term prefix */
  type TypeSelect <: TypeTree

  /** Type tree representing a selection of definition with a given name on a given type prefix */
  type Projection <: TypeTree

  /** Type tree representing a singleton type */
  type Singleton <: TypeTree

  /** Type tree representing a type refinement */
  type Refined <: TypeTree

  /** Type tree representing a type application */
  type Applied <: TypeTree

  /** Type tree representing an annotated type */
  type Annotated <: TypeTree

  /** Type tree representing a type match */
  type MatchTypeTree <: TypeTree

  /** Type tree representing a by name parameter */
  type ByName <: TypeTree

  /** Type tree representing a lambda abstraction type */
  type LambdaTypeTree <: TypeTree

  /** Type tree representing a type binding */
  type TypeBind <: TypeTree

  /** Type tree within a block with aliases `{ type U1 = ... ; T[U1, U2] }` */
  type TypeBlock <: TypeTree

  /** Type tree representing a type bound written in the source */
  type TypeBoundsTree <: Tree /*TypeTree | TypeBoundsTree*/

  /** Type tree representing wildcard type bounds written in the source.
   *  The wildcard type `_` (for example in in `List[_]`) will be a type tree that
   *  represents a type but has `TypeBound`a inside.
   */
  type WildcardTypeTree  <: Tree

  /** Branch of a pattern match or catch clause */
  type CaseDef <: Tree

  /** Branch of a type pattern match */
  type TypeCaseDef <: Tree

  /** Pattern representing a `_ @ _` binding. */
  type Bind <: Tree

  /** Pattern representing a `Xyz(...)` unapply. */
  type Unapply <: Tree

  /** Pattern representing `X | Y | ...` alternatives. */
  type Alternatives <: Tree

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

  /** Type of a reference to a term symbol */
  type TermRef <: Type

  /** Type of a reference to a type symbol */
  type TypeRef <: Type

  /** Type of a `super` reference */
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


  /** Import selectors:
   *   * SimpleSelector: `.bar` in `import foo.bar`
   *   * RenameSelector: `.{bar => baz}` in `import foo.{bar => baz}`
   *   * OmitSelector: `.{bar => _}` in `import foo.{bar => _}`
   */
  type ImportSelector <: AnyRef
  type SimpleSelector <: ImportSelector
  type RenameSelector <: ImportSelector
  type OmitSelector <: ImportSelector

  /** Untyped identifier */
  type Id <: AnyRef

  /** Signature of a method */
  type Signature <: AnyRef

  /** Position in a source file */
  type Position <: AnyRef

  /** Scala source file */
  type SourceFile <: AnyRef

  /** Comment */
  type Comment <: AnyRef

  /** Constant value represented as the constant itself */
  type Constant <: AnyRef

  /** Symbol of a definition.
   *  Then can be compared with == to know if the definition is the same.
   */
  type Symbol <: AnyRef

  /** FlagSet of a Symbol */
  type Flags

  type ImplicitSearchResult <: AnyRef

  type ImplicitSearchSuccess <: ImplicitSearchResult

  type ImplicitSearchFailure <: ImplicitSearchResult

  type DivergingImplicit <: ImplicitSearchFailure

  type NoMatchingImplicits <: ImplicitSearchFailure

  type AmbiguousImplicits <: ImplicitSearchFailure

}
