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
 *           |                             +- Lambda
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

  val kernel: Kernel

  /** Compilation context */
  type Context = kernel.Context

  /** Settings */
  type Settings = kernel.Settings

  /** Tree representing code written in the source */
  type Tree = kernel.Tree

    /** Tree representing a pacakage clause in the source code */
    type PackageClause = kernel.PackageClause

    /** Tree representing a statement in the source code */
    type Statement = kernel.Statement

      /** Tree representing an import in the source code */
      type Import = kernel.Import

      /** Tree representing a definition in the source code. It can be `PackageDef`, `ClassDef`, `TypeDef`, `DefDef` or `ValDef` */
      type Definition = kernel.Definition

        /** Tree representing a package definition. This includes definitions in all source files */
        type PackageDef = kernel.PackageDef

        /** Tree representing a class definition. This includes annonymus class definitions and the class of a module object */
        type ClassDef = kernel.ClassDef

        /** Tree representing a type (paramter or member) definition in the source code */
        type TypeDef = kernel.TypeDef

        /** Tree representing a method definition in the source code */
        type DefDef = kernel.DefDef

        /** Tree representing a value definition in the source code This inclues `val`, `lazy val`, `var`, `object` and parameter defintions. */
        type ValDef = kernel.ValDef

      /** Tree representing an expression in the source code */
      type Term = kernel.Term

        /** Tree representing a reference to definition */
        type Ref = kernel.Ref

          /** Tree representing a reference to definition with a given name */
          type Ident = kernel.Ident

          /** Tree representing a selection of definition with a given name on a given prefix */
          type Select = kernel.Select

        /** Tree representing a literal value in the source code */
        type Literal = kernel.Literal

        /** Tree representing `this` in the source code */
        type This = kernel.This

        /** Tree representing `new` in the source code */
        type New = kernel.New

        /** Tree representing an argument passed with an explicit name. Such as `arg1 = x` in `foo(arg1 = x)` */
        type NamedArg = kernel.NamedArg

        /** Tree an application of arguments. It represents a single list of arguments, multiple argument lists will have nested `Apply`s  */
        type Apply = kernel.Apply

        /** Tree an application of type arguments */
        type TypeApply = kernel.TypeApply

        /** Tree representing `super` in the source code */
        type Super = kernel.Super

        /** Tree representing a type ascription `x: T` in the source code */
        type Typed = kernel.Typed

        /** Tree representing an assignment `x = y` in the source code */
        type Assign = kernel.Assign

        /** Tree representing a block `{ ... }` in the source code */
        type Block = kernel.Block

        /** Tree representing a lambda `(...) => ...` in the source code */
        type Lambda = kernel.Lambda

        /** Tree representing an if/then/else `if (...) ... else ...` in the source code */
        type If = kernel.If

        /** Tree representing a pattern match `x match  { ... }` in the source code */
        type Match = kernel.Match

        /** Tree representing a pattern match `implied match { ... }` in the source code */
        type ImpliedMatch = kernel.ImpliedMatch

        /** Tree representing a try catch `try x catch { ... } finally { ... }` in the source code */
        type Try = kernel.Try

        /** Tree representing a `return` in the source code */
        type Return = kernel.Return

        /** Tree representing a variable argument list in the source code */
        type Repeated = kernel.Repeated

        /** Tree representing the scope of an inlined tree */
        type Inlined = kernel.Inlined

        /** Tree representing a selection of definition with a given name on a given prefix and number of nested scopes of inlined trees */
        type SelectOuter = kernel.SelectOuter

        /** Tree representing a while loop */
        type While = kernel.While

      /** Type tree representing a type written in the source */
      type TypeTree = kernel.TypeTree

        /** Type tree representing an inferred type */
        type Inferred = kernel.Inferred

        /** Type tree representing a reference to definition with a given name */
        type TypeIdent = kernel.TypeIdent

        /** Type tree representing a selection of definition with a given name on a given term prefix */
        type TypeSelect = kernel.TypeSelect

        /** Type tree representing a selection of definition with a given name on a given type prefix */
        type Projection = kernel.Projection

        /** Type tree representing a singleton type */
        type Singleton = kernel.Singleton

        /** Type tree representing a type refinement */
        type Refined = kernel.Refined

        /** Type tree representing a type application */
        type Applied = kernel.Applied

        /** Type tree representing an annotated type */
        type Annotated = kernel.Annotated

        /** Type tree representing a type match */
        type MatchTypeTree = kernel.MatchTypeTree

        /** Type tree representing a by name parameter */
        type ByName = kernel.ByName

        /** Type tree representing a lambda abstraction type */
        type LambdaTypeTree = kernel.LambdaTypeTree

        /** Type tree representing a type binding */
        type TypeBind = kernel.TypeBind

        /** Type tree within a block with aliases `{ type U1 = ... ; T[U1, U2] }` */
        type TypeBlock = kernel.TypeBlock

      /** Type tree representing a type bound written in the source */
      type TypeBoundsTree = kernel.TypeBoundsTree

      /** Type tree representing wildcard type bounds written in the source.
       *  The wildcard type `_` (for example in in `List[_]`) will be a type tree that
       *  represents a type but has `TypeBound`a inside.
       */
      type WildcardTypeTree = kernel.WildcardTypeTree

  /** Branch of a pattern match or catch clause */
  type CaseDef = kernel.CaseDef

  /** Branch of a type pattern match */
  type TypeCaseDef = kernel.TypeCaseDef

  /** Pattern tree of the pattern part of a CaseDef */
  type Pattern = kernel.Pattern

    /** Pattern representing a value. This includes `1`, ```x``` and `_` */
    type Value = kernel.Value

    /** Pattern representing a `_ @ _` binding. */
    type Bind = kernel.Bind

    /** Pattern representing a `Xyz(...)` unapply. */
    type Unapply = kernel.Unapply

    /** Pattern representing `X | Y | ...` alternatives. */
    type Alternatives = kernel.Alternatives

    /** Pattern representing a `x: Y` type test. */
    type TypeTest = kernel.TypeTest

    /** Pattern representing a `_` pattern */
    type WildcardPattern = kernel.WildcardPattern

  /** Type or bounds */
  type TypeOrBounds = kernel.TypeOrBounds

    /** NoPrefix for a type selection */
    type NoPrefix = kernel.NoPrefix

    /** Type bounds */
    type TypeBounds = kernel.TypeBounds

    /** A type */
    type Type = kernel.Type

      /** A singleton type representing a known constant value */
      type ConstantType = kernel.ConstantType

      /** Type of a reference to a symbol */
      type SymRef = kernel.SymRef

      /** Type of a reference to a term */
      type TermRef = kernel.TermRef

      /** Type of a reference to a type */
      type TypeRef = kernel.TypeRef

      /** Type of a `super` refernce */
      type SuperType = kernel.SuperType

      /** A type with a type refinement `T { type U }` */
      type Refinement = kernel.Refinement

      /** A higher kinded type applied to some types `T[U]` */
      type AppliedType = kernel.AppliedType

      /** A type with an anottation `T @foo` */
      type AnnotatedType = kernel.AnnotatedType

      /** Intersection type `T & U` */
      type AndType = kernel.AndType

      /** Union type `T | U` */
      type OrType = kernel.OrType

      /** Type match `T match { case U => ... }` */
      type MatchType = kernel.MatchType

      /** Type of a by by name parameter */
      type ByNameType = kernel.ByNameType

      /** Type of a parameter reference */
      type ParamRef = kernel.ParamRef

      /** Type of `this` */
      type ThisType = kernel.ThisType

      /** A type that is recursively defined `this` */
      type RecursiveThis = kernel.RecursiveThis

      /** A type that is recursively defined */
      type RecursiveType = kernel.RecursiveType

      // TODO can we add the bound back without an cake?
      // TODO is LambdaType really needed? ParamRefExtractor could be split into more precise extractors
      /** Common abstraction for lambda types (MethodType, PolyType and TypeLambda). */
      type LambdaType[ParamInfo /*<: TypeOrBounds*/] = kernel.LambdaType[ParamInfo]

        /** Type of the definition of a method taking a single list of parameters. It's return type may be a MethodType. */
        type MethodType = kernel.MethodType

        /** Type of the definition of a method taking a list of type parameters. It's return type may be a MethodType. */
        type PolyType = kernel.PolyType

        /** Type of the definition of a type lambda taking a list of type parameters. It's return type may be a TypeLambda. */
        type TypeLambda = kernel.TypeLambda


  /** Import selectors:
   *   * SimpleSelector: `.bar` in `import foo.bar`
   *   * RenameSelector: `.{bar => baz}` in `import foo.{bar => baz}`
   *   * OmitSelector: `.{bar => _}` in `import foo.{bar => _}`
   */
  type ImportSelector = kernel.ImportSelector
  type SimpleSelector = kernel.SimpleSelector
  type RenameSelector = kernel.RenameSelector
  type OmitSelector = kernel.OmitSelector

  /** Untyped identifier */
  type Id = kernel.Id

  /** JVM signature of a method */
  type Signature = kernel.Signature

  /** Position in a source file */
  type Position = kernel.Position

  /** Scala source file */
  type SourceFile = kernel.SourceFile

  /** Comment */
  type Comment = kernel.Comment

  /** Constant value represented as the constant itself */
  type Constant = kernel.Constant

  /** Symbol of a definition.
   *  Then can be compared with == to know if the definition is the same.
   */
  type Symbol = kernel.Symbol

    /** Symbol of a package definition */
    type PackageDefSymbol = kernel.PackageDefSymbol

    /** Symbol representing a type definition. */
    type TypeSymbol = kernel.TypeSymbol

      /** Symbol of a class definition. This includes anonymous class definitions and the class of a module object. */
      type ClassDefSymbol = kernel.ClassDefSymbol

      /** Symbol of a type (parameter or member) definition. */
      type TypeDefSymbol = kernel.TypeDefSymbol

      /** Symbol representing a type bind definition. */
      type TypeBindSymbol = kernel.TypeBindSymbol

    /** Symbol representing a term definition. */
    type TermSymbol = kernel.TermSymbol

      /** Symbol representing a method definition. */
      type DefDefSymbol = kernel.DefDefSymbol

      /** Symbol representing a value definition. This includes `val`, `lazy val`, `var`, `object` and parameter definitions. */
      type ValDefSymbol = kernel.ValDefSymbol

      /** Symbol representing a bind definition. */
      type BindSymbol = kernel.BindSymbol

    /** No symbol available. */
    type NoSymbol = kernel.NoSymbol

  /** FlagSet of a Symbol */
  type Flags = kernel.Flags
}
