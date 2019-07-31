package scala.tasty.reflect

// Keep doc in syncwith docs/docs/reference/tasty-reflect.md
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
trait Core {

  private[scala] val internal: Internal

  /** Compilation context */
  type Context = internal.Context

  /** Settings */
  type Settings = internal.Settings

  /** Tree representing code written in the source */
  type Tree = internal.Tree

    /** Tree representing a pacakage clause in the source code */
    type PackageClause = internal.PackageClause

    /** Tree representing a statement in the source code */
    type Statement = internal.Statement

      /** Tree representing an import in the source code */
      type Import = internal.Import

      /** Tree representing a definition in the source code. It can be `PackageDef`, `ClassDef`, `TypeDef`, `DefDef` or `ValDef` */
      type Definition = internal.Definition

        /** Tree representing a package definition. This includes definitions in all source files */
        type PackageDef = internal.PackageDef

        /** Tree representing a class definition. This includes annonymus class definitions and the class of a module object */
        type ClassDef = internal.ClassDef

        /** Tree representing a type (paramter or member) definition in the source code */
        type TypeDef = internal.TypeDef

        /** Tree representing a method definition in the source code */
        type DefDef = internal.DefDef

        /** Tree representing a value definition in the source code This inclues `val`, `lazy val`, `var`, `object` and parameter defintions. */
        type ValDef = internal.ValDef

      /** Tree representing an expression in the source code */
      type Term = internal.Term

        /** Tree representing a reference to definition */
        type Ref = internal.Ref

          /** Tree representing a reference to definition with a given name */
          type Ident = internal.Ident

          /** Tree representing a selection of definition with a given name on a given prefix */
          type Select = internal.Select

        /** Tree representing a literal value in the source code */
        type Literal = internal.Literal

        /** Tree representing `this` in the source code */
        type This = internal.This

        /** Tree representing `new` in the source code */
        type New = internal.New

        /** Tree representing an argument passed with an explicit name. Such as `arg1 = x` in `foo(arg1 = x)` */
        type NamedArg = internal.NamedArg

        /** Tree an application of arguments. It represents a single list of arguments, multiple argument lists will have nested `Apply`s  */
        type Apply = internal.Apply

        /** Tree an application of type arguments */
        type TypeApply = internal.TypeApply

        /** Tree representing `super` in the source code */
        type Super = internal.Super

        /** Tree representing a type ascription `x: T` in the source code */
        type Typed = internal.Typed

        /** Tree representing an assignment `x = y` in the source code */
        type Assign = internal.Assign

        /** Tree representing a block `{ ... }` in the source code */
        type Block = internal.Block

        /** A lambda `(...) => ...` in the source code is represented as
         *  a local method and a closure:
         *
         *  {
         *    def m(...) = ...
         *    closure(m)
         *  }
         *
         */
        type Closure = internal.Closure

        /** Tree representing an if/then/else `if (...) ... else ...` in the source code */
        type If = internal.If

        /** Tree representing a pattern match `x match  { ... }` in the source code */
        type Match = internal.Match

        /** Tree representing a pattern match `delegate match { ... }` in the source code */
        type ImpliedMatch = internal.ImpliedMatch

        /** Tree representing a try catch `try x catch { ... } finally { ... }` in the source code */
        type Try = internal.Try

        /** Tree representing a `return` in the source code */
        type Return = internal.Return

        /** Tree representing a variable argument list in the source code */
        type Repeated = internal.Repeated

        /** Tree representing the scope of an inlined tree */
        type Inlined = internal.Inlined

        /** Tree representing a selection of definition with a given name on a given prefix and number of nested scopes of inlined trees */
        type SelectOuter = internal.SelectOuter

        /** Tree representing a while loop */
        type While = internal.While

      /** Type tree representing a type written in the source */
      type TypeTree = internal.TypeTree

        /** Type tree representing an inferred type */
        type Inferred = internal.Inferred

        /** Type tree representing a reference to definition with a given name */
        type TypeIdent = internal.TypeIdent

        /** Type tree representing a selection of definition with a given name on a given term prefix */
        type TypeSelect = internal.TypeSelect

        /** Type tree representing a selection of definition with a given name on a given type prefix */
        type Projection = internal.Projection

        /** Type tree representing a singleton type */
        type Singleton = internal.Singleton

        /** Type tree representing a type refinement */
        type Refined = internal.Refined

        /** Type tree representing a type application */
        type Applied = internal.Applied

        /** Type tree representing an annotated type */
        type Annotated = internal.Annotated

        /** Type tree representing a type match */
        type MatchTypeTree = internal.MatchTypeTree

        /** Type tree representing a by name parameter */
        type ByName = internal.ByName

        /** Type tree representing a lambda abstraction type */
        type LambdaTypeTree = internal.LambdaTypeTree

        /** Type tree representing a type binding */
        type TypeBind = internal.TypeBind

        /** Type tree within a block with aliases `{ type U1 = ... ; T[U1, U2] }` */
        type TypeBlock = internal.TypeBlock

      /** Type tree representing a type bound written in the source */
      type TypeBoundsTree = internal.TypeBoundsTree

      /** Type tree representing wildcard type bounds written in the source.
       *  The wildcard type `_` (for example in in `List[_]`) will be a type tree that
       *  represents a type but has `TypeBound`a inside.
       */
      type WildcardTypeTree = internal.WildcardTypeTree

  /** Branch of a pattern match or catch clause */
  type CaseDef = internal.CaseDef

  /** Branch of a type pattern match */
  type TypeCaseDef = internal.TypeCaseDef

  /** Pattern tree of the pattern part of a CaseDef */
  type Pattern = internal.Pattern

    /** Pattern representing a value. This includes `1`, ```x``` and `_` */
    type Value = internal.Value

    /** Pattern representing a `_ @ _` binding. */
    type Bind = internal.Bind

    /** Pattern representing a `Xyz(...)` unapply. */
    type Unapply = internal.Unapply

    /** Pattern representing `X | Y | ...` alternatives. */
    type Alternatives = internal.Alternatives

    /** Pattern representing a `x: Y` type test. */
    type TypeTest = internal.TypeTest

    /** Pattern representing a `_` pattern */
    type WildcardPattern = internal.WildcardPattern

  /** Type or bounds */
  type TypeOrBounds = internal.TypeOrBounds

    /** NoPrefix for a type selection */
    type NoPrefix = internal.NoPrefix

    /** Type bounds */
    type TypeBounds = internal.TypeBounds

    /** A type */
    type Type = internal.Type

      /** A singleton type representing a known constant value */
      type ConstantType = internal.ConstantType

      /** Type of a reference to a symbol */
      type SymRef = internal.SymRef

      /** Type of a reference to a term */
      type TermRef = internal.TermRef

      /** Type of a reference to a type */
      type TypeRef = internal.TypeRef

      /** Type of a `super` refernce */
      type SuperType = internal.SuperType

      /** A type with a type refinement `T { type U }` */
      type Refinement = internal.Refinement

      /** A higher kinded type applied to some types `T[U]` */
      type AppliedType = internal.AppliedType

      /** A type with an anottation `T @foo` */
      type AnnotatedType = internal.AnnotatedType

      /** Intersection type `T & U` */
      type AndType = internal.AndType

      /** Union type `T | U` */
      type OrType = internal.OrType

      /** Type match `T match { case U => ... }` */
      type MatchType = internal.MatchType

      /** Type of a by by name parameter */
      type ByNameType = internal.ByNameType

      /** Type of a parameter reference */
      type ParamRef = internal.ParamRef

      /** Type of `this` */
      type ThisType = internal.ThisType

      /** A type that is recursively defined `this` */
      type RecursiveThis = internal.RecursiveThis

      /** A type that is recursively defined */
      type RecursiveType = internal.RecursiveType

      // TODO can we add the bound back without an cake?
      // TODO is LambdaType really needed? ParamRefExtractor could be split into more precise extractors
      /** Common abstraction for lambda types (MethodType, PolyType and TypeLambda). */
      type LambdaType[ParamInfo /*<: TypeOrBounds*/] = internal.LambdaType[ParamInfo]

        /** Type of the definition of a method taking a single list of parameters. It's return type may be a MethodType. */
        type MethodType = internal.MethodType

        /** Type of the definition of a method taking a list of type parameters. It's return type may be a MethodType. */
        type PolyType = internal.PolyType

        /** Type of the definition of a type lambda taking a list of type parameters. It's return type may be a TypeLambda. */
        type TypeLambda = internal.TypeLambda


  /** Import selectors:
   *   * SimpleSelector: `.bar` in `import foo.bar`
   *   * RenameSelector: `.{bar => baz}` in `import foo.{bar => baz}`
   *   * OmitSelector: `.{bar => _}` in `import foo.{bar => _}`
   */
  type ImportSelector = internal.ImportSelector
  type SimpleSelector = internal.SimpleSelector
  type RenameSelector = internal.RenameSelector
  type OmitSelector = internal.OmitSelector

  /** Untyped identifier */
  type Id = internal.Id

  /** JVM signature of a method */
  type Signature = internal.Signature

  /** Position in a source file */
  type Position = internal.Position

  /** Scala source file */
  type SourceFile = internal.SourceFile

  /** Comment */
  type Comment = internal.Comment

  /** Constant value represented as the constant itself */
  type Constant = internal.Constant

  /** Symbol of a definition.
   *  Then can be compared with == to know if the definition is the same.
   */
  type Symbol = internal.Symbol

    /** Symbol of a package definition */
    type PackageDefSymbol = internal.PackageDefSymbol

    /** Symbol representing a type definition. */
    type TypeSymbol = internal.TypeSymbol

      /** Symbol of a class definition. This includes anonymous class definitions and the class of a module object. */
      type ClassDefSymbol = internal.ClassDefSymbol

      /** Symbol of a type (parameter or member) definition. */
      type TypeDefSymbol = internal.TypeDefSymbol

      /** Symbol representing a type bind definition. */
      type TypeBindSymbol = internal.TypeBindSymbol

    /** Symbol representing a term definition. */
    type TermSymbol = internal.TermSymbol

      /** Symbol representing a method definition. */
      type DefDefSymbol = internal.DefDefSymbol

      /** Symbol representing a value definition. This includes `val`, `lazy val`, `var`, `object` and parameter definitions. */
      type ValDefSymbol = internal.ValDefSymbol

      /** Symbol representing a bind definition. */
      type BindSymbol = internal.BindSymbol

    /** No symbol available. */
    type NoSymbol = internal.NoSymbol

  /** FlagSet of a Symbol */
  type Flags = internal.Flags

  type ImplicitSearchResult = internal.ImplicitSearchResult

  type ImplicitSearchSuccess = internal.ImplicitSearchSuccess

  type ImplicitSearchFailure = internal.ImplicitSearchFailure

  type DivergingImplicit = internal.DivergingImplicit

  type NoMatchingImplicits = internal.NoMatchingImplicits

  type AmbiguousImplicits = internal.AmbiguousImplicits

}
