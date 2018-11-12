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
 *                         +- Term --------+- Ident
 *                                         +- Select
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
 *                                         +- DoWhile
 *
 *
 *                         +- TypeTree ----+- Synthetic
 *                         |               +- Ident
 *                         |               +- Select
 *                         |               +- Project
 *                         |               +- Singleton
 *  +- TypeOrBoundsTree ---+               +- Refined
 *                         |               +- Applied
 *                         |               +- Annotated
 *                         |               +- And
 *                         |               +- Or
 *                         |               +- MatchType
 *                         |               +- ByName
 *                         |               +- TypeLambdaTree
 *                         |               +- Bind
 *                         |
 *                         +- TypeBoundsTree
 *                         +- SyntheticBounds
 *
 *  +- CaseDef
 *  +- TypeCaseDef
 *
 *  +- Pattern --+- Value
 *               +- Bind
 *               +- Unapply
 *               +- Alternative
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
 *  Aliases:
 *   # TermOrTypeTree = Term | TypeTree
 *
 *  ```
 */
trait Core {

  /** Compilation context */
  type Context

  /** Settings */
  type Settings

  // TODO: When bootstrapped, remove and use `Term | TypeTree` type directly in other files
  /** Workaround missing `|` types in Scala 2 to represent `Term | TypeTree` */
  type TermOrTypeTree /* Term | TypeTree */

  /** Tree representing executable code written in the source */
  type Tree

    /** Tree representing a pacakage clause in the source code */
    type PackageClause <: Tree

    /** Tree representing a statement in the source code */
    type Statement <: Tree

      /** Tree representing an import in the source code */
      type Import <: Statement

      /** Tree representing a definition in the source code. It can be `PackageDef`, `ClassDef`, `TypeDef`, `DefDef` or `ValDef`*/
      type Definition <: Statement

        /** Tree representing a package definition. This includes definitions in all source files. */
        type PackageDef <: Definition

        /** Tree representing a class definition. This includes annonymus class definitions and the class of a module object. */
        type ClassDef <: Definition

        /** Tree representing a type (paramter or member) definition in the source code. */
        type TypeDef <: Definition

        /** Tree representing a method definition in the source code. */
        type DefDef <: Definition

        /** Tree representing a value definition in the source code. This inclues `val`, `lazy val`, `var`, `object` and parameter defintions. */
        type ValDef <: Definition

      /** Tree representing an expression in the source code. */
      type Term <: Statement

        // TODO Add subtype types of Term for documentation? Or support refined bindings and add the types.


  /** Branch of a pattern match or catch clause */
  type CaseDef

  /** Branch of a type pattern match */
  type TypeCaseDef

  /** Pattern tree of the pattern part of a CaseDef */
  type Pattern

    /** Pattern representing a value. This includes `1`, ```x``` and `_` */
    type Value <: Pattern

    /** Pattern representing a `_ @ _` binding. */
    type Bind <: Pattern

    /** Pattern representing a `Xyz(...)` unapply. */
    type Unapply <: Pattern

    /** Pattern representing `X | Y | ...` alternatives. */
    type Alternative <: Pattern

    /** Pattern representing a `x: Y` type test. */
    type TypeTest <: Pattern

  /** Type tree representing a type or a bounds written in the source */
  type TypeOrBoundsTree

    /** Type tree representing a type written in the source */
    type TypeTree <: TypeOrBoundsTree

      // TODO Add subtype types of TypeTree for documentation? Or support refined bindings and add the types.


    /** Type tree representing a type bound written in the source */
    type TypeBoundsTree <: TypeOrBoundsTree

  /** Type or bounds */
  type TypeOrBounds

    /** NoPrefix for a type selection */
    type NoPrefix <: TypeOrBounds

    /** Type bounds */
    type TypeBounds <: TypeOrBounds

    /** A type */
    type Type <: TypeOrBounds

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
  type ImportSelector

  /** Untyped identifier */
  type Id

  /** JVM signature of a method */
  type Signature

  /** Source position */
  type Position

  /** Constant value represented as the constant itself */
  type Constant

  /** Symbol of a definition.
   *  Then can be compared with == to know if the definition is the same.
   */
  type Symbol

    /** Symbol of a package defnition */
    type PackageSymbol <: Symbol

    /** Symbol of a class defnition. This includes annonymus class definitions and the class of a module object. */
    type ClassSymbol <: Symbol

    /** Symbol of a type (paramter or member) definition. */
    type TypeSymbol <: Symbol

    /** Symbol representing a method definition. */
    type DefSymbol <: Symbol

    /** Symbol representing a value definition. This inclues `val`, `lazy val`, `var`, `object` and parameter defintions. */
    type ValSymbol <: Symbol

    /** Symbol representing a bind definition. */
    type BindSymbol <: Symbol

    /** No symbol availabe. */
    type NoSymbol <: Symbol

}
