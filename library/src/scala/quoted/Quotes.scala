package scala.quoted

import scala.reflect.TypeTest

/** Current Quotes in scope */
inline def quotes(using q: Quotes): q.type = q

/** Quotation context provided by a macro expansion or in the scope of `scala.quoted.run`.
 *  Used to perform all operations on quoted `Expr` or `Type`.
 *
 *  It contains the low-level Typed AST API metaprogramming API.
 *  This API does not have the static type guarantiees that `Expr` and `Type` provide.
 *
 *  @param tasty Typed AST API. Usage: `def f(qctx: Quotes) = { import quotes.reflect._; ... }`.
 */
trait Quotes { self: runtime.QuoteUnpickler & runtime.QuoteMatching =>

  // Extension methods for `Expr[T]`
  extension [T](self: Expr[T]):
    /** Show a source code like representation of this expression without syntax highlight */
    def show: String

    /** Shows the tree as fully typed source code colored with ANSI */
    def showAnsiColored: String

    /** Pattern matches `this` against `that`. Effectively performing a deep equality check.
    *  It does the equivalent of
    *  ```
    *  this match
    *    case '{...} => true // where the contents of the pattern are the contents of `that`
    *    case _ => false
    *  ```
    */
    def matches(that: Expr[Any]): Boolean

    /** Return the unlifted value of this expression.
     *
     *  Returns `None` if the expression does not contain a value or contains side effects.
     *  Otherwise returns the `Some` of the value.
     */
    def unlift(using Unliftable[T]): Option[T] =
      summon[Unliftable[T]].fromExpr(self)(using Quotes.this)

    /** Return the unlifted value of this expression.
     *
     *  Emits an error and throws if the expression does not contain a value or contains side effects.
     *  Otherwise returns the value.
     */
    def unliftOrError(using Unliftable[T]): T =
      def reportError =
        val msg = s"Expected a known value. \n\nThe value of: ${self.show}\ncould not be unlifted using $unlift"
        reflect.report.throwError(msg, self)
      summon[Unliftable[T]].fromExpr(self)(using Quotes.this).getOrElse(reportError)

  end extension

  // Extension methods for `Expr[Any]` that take another explicit type parameter
  extension [X](self: Expr[Any]):
    /** Checks is the `quoted.Expr[?]` is valid expression of type `X` */
    def isExprOf(using Type[X]): Boolean

    /** Convert this to an `quoted.Expr[X]` if this expression is a valid expression of type `X` or throws */
    def asExprOf(using Type[X]): Expr[X]
  end extension

  /** Low-level Typed AST API metaprogramming API.
   *  This API does not have the static type guarantiees that `Expr` and `Type` provide.
   */
  val reflect: Reflection

  /** AST reflection interface.
   *
   *  Provides all functionality related with AST based metaprogramming.
   *
   *  Each type `XYZ` in the API is defined as an abstract type `type XYZ`.
   *  Methods on `XYZ` are provided by a `given XYZMethods` which implements extension methods on `XYZ` in the trait `XYZMethods`.
   *  The `XYZ` module is defined by a `val XYZ: XYZModule` which contains the methods defined in `XYZModule`.
   *  Type tests (`TypeTest`) are also given to perform subtype checks on these types.
   *
   *  Type hierarchy
   *  ```none
   *
   *  +- Tree -+- PackageClause
   *           +- Import
   *           +- Statement -+- Definition --+- ClassDef
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
   *           |                             +- SummonFrom
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
   *           |               +- TypeProjection
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
   *  +- TypeRepr -+- ConstantType
   *               +- TermRef
   *               +- TypeRef
   *               +- SuperType
   *               +- Refinement
   *               +- AppliedType
   *               +- AnnotatedType
   *               +- AndType
   *               +- OrType
   *               +- MatchType
   *               +- ByNameType
   *               +- ParamRef
   *               +- ThisType
   *               +- RecursiveThis
   *               +- RecursiveType
   *               +- LambdaType -+- MethodType
   *               |              +- PolyType
   *               |              +- TypeLambda
   *               +- TypeBounds
   *               +- NoPrefix
   *
   *  +- ImportSelector -+- SimpleSelector
   *                     +- RenameSelector
   *                     +- OmitSelector
   *                     +- GivenSelector
   *
   *  +- Signature
   *
   *  +- Position
   *
   *  +- Documentation
   *
   *  +- Constant
   *
   *  +- Symbol
   *
   *  +- Flags
   *
   *  ```
   */
  trait Reflection { self: reflect.type =>

    ///////////////
    //   TREES   //
    ///////////////


    /** Tree representing code written in the source */
    type Tree <: AnyRef

    /** Module object of `type Tree`  */
    val Tree: TreeModule

    /** Methods of the module object `val Tree` */
    trait TreeModule { this: Tree.type =>
      /** Returns the Term representation this expression */
      def of(expr: Expr[Any]): Tree
    }

    /** Makes extension methods on `Tree` available without any imports */
    given TreeMethods as TreeMethods = TreeMethodsImpl

    /** Implementation of extension methods on `Tree` */
    protected val TreeMethodsImpl: TreeMethods

    /** Extension methods of `Tree` */
    trait TreeMethods {

      extension (self: Tree):
        /** Position in the source code */
        def pos: Position

        /** Symbol of defined or referred by this tree */
        def symbol: Symbol

        /** Shows the tree as extractors */
        def showExtractors: String

        /** Shows the tree as fully typed source code */
        def show: String

        /** Shows the tree as fully typed source code colored with ANSI */
        def showAnsiColored: String

        /** Does this tree represent a valid expression? */
        def isExpr: Boolean

        /** Convert this tree to an `quoted.Expr[Any]` if the tree is a valid expression or throws */
        def asExpr: Expr[Any]
      end extension

      /** Convert this tree to an `quoted.Expr[T]` if the tree is a valid expression or throws */
      extension [T](self: Tree)
        def asExprOf(using Type[T]): Expr[T]

      extension [ThisTree <: Tree](self: ThisTree):
        /** Changes the owner of the symbols in the tree */
        def changeOwner(newOwner: Symbol): ThisTree
      end extension

    }

    /** Tree representing a pacakage clause in the source code */
    type PackageClause <: Tree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `PackageClause` */
    given TypeTest[Tree, PackageClause] = PackageClauseTypeTest

    /** Implementation of `TypeTest[Tree, PackageClause]` */
    protected val PackageClauseTypeTest: TypeTest[Tree, PackageClause]

    /** Module object of `type PackageClause`  */
    val PackageClause: PackageClauseModule

    /** Methods of the module object `val PackageClause` */
    trait PackageClauseModule { this: PackageClause.type =>
      def apply(pid: Ref, stats: List[Tree]): PackageClause
      def copy(original: Tree)(pid: Ref, stats: List[Tree]): PackageClause
      def unapply(tree: PackageClause): Some[(Ref, List[Tree])]
    }

    /** Makes extension methods on `PackageClause` available without any imports */
    given PackageClauseMethods as PackageClauseMethods = PackageClauseMethodsImpl

    /** Implementation of extension methods on `PackageClause` */
    protected val PackageClauseMethodsImpl: PackageClauseMethods

    /** Extension methods of `PackageClause` */
    trait PackageClauseMethods:
      extension (self: PackageClause):
        def pid: Ref
        def stats: List[Tree]
      end extension
    end PackageClauseMethods

    /** Tree representing an import in the source code */
    type Import <: Statement

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is an `Import` */
    given TypeTest[Tree, Import] = ImportTypeTest

    /** Implementation of `TypeTest[Tree, Import]` */
    protected val ImportTypeTest: TypeTest[Tree, Import]

    /** Module object of `type Import`  */
    val Import: ImportModule

    /** Methods of the module object `val Import` */
    trait ImportModule { this: Import.type =>
      def apply(expr: Term, selectors: List[ImportSelector]): Import
      def copy(original: Tree)(expr: Term, selectors: List[ImportSelector]): Import
      def unapply(tree: Import): Option[(Term, List[ImportSelector])]
    }

    /** Makes extension methods on `Import` available without any imports */
    given ImportMethods as ImportMethods = ImportMethodsImpl

    /** Implementation of extension methods on `Import` */
    protected val ImportMethodsImpl: ImportMethods

    /** Extension methods of `Import` */
    trait ImportMethods:
      extension (self: Import):
        def expr: Term
        def selectors: List[ImportSelector]
      end extension
    end ImportMethods

    /** Tree representing a statement in the source code */
    type Statement <: Tree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Statement` */
    given TypeTest[Tree, Statement] = StatementTypeTest

    /** Implementation of `TypeTest[Tree, Statement]` */
    protected val StatementTypeTest: TypeTest[Tree, Statement]

    // ----- Definitions ----------------------------------------------

    /** Tree representing a definition in the source code. It can be `ClassDef`, `TypeDef`, `DefDef` or `ValDef` */
    type Definition <: Statement

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Definition` */
    given TypeTest[Tree, Definition] = DefinitionTypeTest

    /** Implementation of `TypeTest[Tree, Definition]` */
    protected val DefinitionTypeTest: TypeTest[Tree, Definition]

    /** Module object of `type Definition`  */
    val Definition: DefinitionModule

    /** Methods of the module object `val Definition` */
    trait DefinitionModule { this: Definition.type => }

    /** Makes extension methods on `Definition` available without any imports */
    given DefinitionMethods as DefinitionMethods = DefinitionMethodsImpl

    /** Implementation of extension methods on `Definition` */
    protected val DefinitionMethodsImpl: DefinitionMethods

    /** Extension methods of `Definition` */
    trait DefinitionMethods:
      extension (self: Definition):
        def name: String
      end extension
    end DefinitionMethods

    // ClassDef

    /** Tree representing a class definition. This includes anonymous class definitions and the class of a module object */
    type ClassDef <: Definition

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `ClassDef` */
    given TypeTest[Tree, ClassDef] = ClassDefTypeTest

    /** Implementation of `TypeTest[Tree, ClassDef]` */
    protected val ClassDefTypeTest: TypeTest[Tree, ClassDef]

    /** Module object of `type ClassDef`  */
    val ClassDef: ClassDefModule

    /** Methods of the module object `val ClassDef` */
    trait ClassDefModule { this: ClassDef.type =>
      // TODO def apply(name: String, constr: DefDef, parents: List[TermOrTypeTree], selfOpt: Option[ValDef], body: List[Statement]): ClassDef
      def copy(original: Tree)(name: String, constr: DefDef, parents: List[Tree /* Term | TypeTree */], derived: List[TypeTree], selfOpt: Option[ValDef], body: List[Statement]): ClassDef
      def unapply(cdef: ClassDef): Option[(String, DefDef, List[Tree /* Term | TypeTree */], List[TypeTree], Option[ValDef], List[Statement])]
    }

    /** Makes extension methods on `ClassDef` available without any imports */
    given ClassDefMethods as ClassDefMethods = ClassDefMethodsImpl

    /** Implementation of extension methods on `ClassDef` */
    protected val ClassDefMethodsImpl: ClassDefMethods

    /** Extension methods of `ClassDef` */
    trait ClassDefMethods:
      extension (self: ClassDef):
        def constructor: DefDef
        def parents: List[Tree /* Term | TypeTree */]
        def derived: List[TypeTree]
        def self: Option[ValDef]
        def body: List[Statement]
      end extension
    end ClassDefMethods

    // DefDef

    /** Tree representing a method definition in the source code */
    type DefDef <: Definition

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `DefDef` */
    given TypeTest[Tree, DefDef] = DefDefTypeTest

    /** Implementation of `TypeTest[Tree, DefDef]` */
    protected val DefDefTypeTest: TypeTest[Tree, DefDef]

    /** Module object of `type DefDef`  */
    val DefDef: DefDefModule

    /** Methods of the module object `val DefDef` */
    trait DefDefModule { this: DefDef.type =>
      def apply(symbol: Symbol, rhsFn: List[TypeRepr] => List[List[Term]] => Option[Term]): DefDef
      def copy(original: Tree)(name: String, typeParams: List[TypeDef], paramss: List[List[ValDef]], tpt: TypeTree, rhs: Option[Term]): DefDef
      def unapply(ddef: DefDef): Option[(String, List[TypeDef], List[List[ValDef]], TypeTree, Option[Term])]
    }

    /** Makes extension methods on `DefDef` available without any imports */
    given DefDefMethods as DefDefMethods = DefDefMethodsImpl

    /** Implementation of extension methods on `DefDef` */
    protected val DefDefMethodsImpl: DefDefMethods

    /** Extension methods of `DefDef` */
    trait DefDefMethods:
      extension (self: DefDef):
        def typeParams: List[TypeDef]
        def paramss: List[List[ValDef]]
        def returnTpt: TypeTree
        def rhs: Option[Term]
      end extension
    end DefDefMethods

    // ValDef

    /** Tree representing a value definition in the source code This inclues `val`, `lazy val`, `var`, `object` and parameter defintions. */
    type ValDef <: Definition

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `ValDef` */
    given TypeTest[Tree, ValDef] = ValDefTypeTest

    /** Implementation of `TypeTest[Tree, ValDef]` */
    protected val ValDefTypeTest: TypeTest[Tree, ValDef]

    /** Module object of `type ValDef`  */
    val ValDef: ValDefModule

    /** Methods of the module object `val ValDef` */
    trait ValDefModule { this: ValDef.type =>
      def apply(symbol: Symbol, rhs: Option[Term]): ValDef
      def copy(original: Tree)(name: String, tpt: TypeTree, rhs: Option[Term]): ValDef
      def unapply(vdef: ValDef): Option[(String, TypeTree, Option[Term])]

      /** Creates a block `{ val <name> = <rhs: Term>; <body(x): Term> }` */
      def let(owner: Symbol, name: String, rhs: Term)(body: Ident => Term): Term

      /** Creates a block `{ val x = <rhs: Term>; <body(x): Term> }` */
      def let(owner: Symbol, rhs: Term)(body: Ident => Term): Term =
        let(owner, "x", rhs)(body)

      /** Creates a block `{ val x1 = <terms(0): Term>; ...; val xn = <terms(n-1): Term>; <body(List(x1, ..., xn)): Term> }` */
      def let(owner: Symbol, terms: List[Term])(body: List[Ident] => Term): Term
    }

    /** Makes extension methods on `ValDef` available without any imports */
    given ValDefMethods as ValDefMethods = ValDefMethodsImpl

    /** Implementation of extension methods on `ValDef` */
    protected val ValDefMethodsImpl: ValDefMethods

    /** Extension methods of `ValDef` */
    trait ValDefMethods:
      extension (self: ValDef):
        def tpt: TypeTree
        def rhs: Option[Term]
      end extension
    end ValDefMethods

    // TypeDef

    /** Tree representing a type (parameter or member) definition in the source code */
    type TypeDef <: Definition

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `TypeDef` */
    given TypeTest[Tree, TypeDef] = TypeDefTypeTest

    /** Implementation of `TypeTest[Tree, TypeDef]` */
    protected val TypeDefTypeTest: TypeTest[Tree, TypeDef]

    /** Module object of `type TypeDef`  */
    val TypeDef: TypeDefModule

    /** Methods of the module object `val TypeDef` */
    trait TypeDefModule { this: TypeDef.type =>
      def apply(symbol: Symbol): TypeDef
      def copy(original: Tree)(name: String, rhs: Tree /*TypeTree | TypeBoundsTree*/): TypeDef
      def unapply(tdef: TypeDef): Option[(String, Tree /*TypeTree | TypeBoundsTree*/ /* TypeTree | TypeBoundsTree */)]
    }

    /** Makes extension methods on `TypeDef` available without any imports */
    given TypeDefMethods as TypeDefMethods = TypeDefMethodsImpl

    /** Implementation of extension methods on `TypeDef` */
    protected val TypeDefMethodsImpl: TypeDefMethods

    /** Extension methods of `TypeDef` */
    trait TypeDefMethods:
      extension (self: TypeDef):
        def rhs: Tree /*TypeTree | TypeBoundsTree*/
      end extension
    end TypeDefMethods


    // ----- Terms ----------------------------------------------------

    /** Tree representing an expression in the source code */
    type Term <: Statement

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Term` */
    given TypeTest[Tree, Term] = TermTypeTest

    /** Implementation of `TypeTest[Tree, Term]` */
    protected val TermTypeTest: TypeTest[Tree, Term]

    /** Module object of `type Term`  */
    val Term: TermModule

    /** Methods of the module object `val Term` */
    trait TermModule { this: Term.type =>

      /** Returns the Term representation this expression */
      def of(expr: Expr[Any]): Term

      /** Returns a term that is functionally equivalent to `t`,
      *  however if `t` is of the form `((y1, ..., yn) => e2)(e1, ..., en)`
      *  then it optimizes this the top most call by returning the `Some`
      *  with the result of beta-reducing the application.
      *  Otherwise returns `None`.
      *
      *   To retain semantics the argument `ei` is bound as `val yi = ei` and by-name arguments to `def yi = ei`.
      *   Some bindings may be elided as an early optimization.
      */
      def betaReduce(term: Term): Option[Term]

    }

    /** Makes extension methods on `Term` available without any imports */
    given TermMethods as TermMethods = TermMethodsImpl

    /** Implementation of extension methods on `Term` */
    protected val TermMethodsImpl: TermMethods

    /** Extension methods of `Term` */
    trait TermMethods {
      extension (self: Term):

        /** TypeRepr of this term */
        def tpe: TypeRepr

        /** Replace Inlined nodes and InlineProxy references to underlying arguments */
        def underlyingArgument: Term

        /** Replace Ident nodes references to the underlying tree that defined them */
        def underlying: Term

        /** Converts a partally applied term into a lambda expression */
        def etaExpand(owner: Symbol): Term

        /** A unary apply node with given argument: `tree(arg)` */
        def appliedTo(arg: Term): Term

        /** An apply node with given arguments: `tree(arg, args0, ..., argsN)` */
        def appliedTo(arg: Term, args: Term*): Term

        /** An apply node with given argument list `tree(args(0), ..., args(args.length - 1))` */
        def appliedToArgs(args: List[Term]): Apply

        /** The current tree applied to given argument lists:
        *  `tree (argss(0)) ... (argss(argss.length -1))`
        */
        def appliedToArgss(argss: List[List[Term]]): Term

        /** The current tree applied to (): `tree()` */
        def appliedToNone: Apply

        /** The current tree applied to given type argument: `tree[targ]` */
        def appliedToType(targ: TypeRepr): Term

        /** The current tree applied to given type arguments: `tree[targ0, ..., targN]` */
        def appliedToTypes(targs: List[TypeRepr]): Term

        /** The current tree applied to given type argument list: `tree[targs(0), ..., targs(targs.length - 1)]` */
        def appliedToTypeTrees(targs: List[TypeTree]): Term

        /** A select node that selects the given symbol. */
        def select(sym: Symbol): Select

      end extension

    }

    /** Tree representing a reference to definition */
    type Ref <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Ref` */
    given TypeTest[Tree, Ref] = RefTypeTest

    /** Implementation of `TypeTest[Tree, Ref]` */
    protected val RefTypeTest: TypeTest[Tree, Ref]

    /** Module object of `type Ref`  */
    val Ref: RefModule

    /** Methods of the module object `val Ref` */
    trait RefModule { this: Ref.type =>

      /** A tree representing the same reference as the given type */
      def term(tp: TermRef): Ref

      /** Create a reference tree from a symbol
      *
      *  If `sym` refers to a class member `foo` in class `C`,
      *  returns a tree representing `C.this.foo`.
      *
      *  If `sym` refers to a local definition `foo`, returns
      *  a tree representing `foo`.
      *
      *  @note In both cases, the constructed tree should only
      *  be spliced into the places where such accesses make sense.
      *  For example, it is incorrect to have `C.this.foo` outside
      *  the class body of `C`, or have `foo` outside the lexical
      *  scope for the definition of `foo`.
      */
      def apply(sym: Symbol): Ref
    }

    /** Tree representing a reference to definition with a given name */
    type Ident <: Ref

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is an `Ident` */
    given TypeTest[Tree, Ident] = IdentTypeTest

    /** Implementation of `TypeTest[Tree, Ident]` */
    protected val IdentTypeTest: TypeTest[Tree, Ident]

    /** Module object of `type Ident`  */
    val Ident: IdentModule

    /** Methods of the module object `val Ident` */
    trait IdentModule { this: Ident.type =>
      def apply(tmref: TermRef): Term

      def copy(original: Tree)(name: String): Ident

      /** Matches a term identifier and returns its name */
      def unapply(tree: Ident): Option[String]
    }

    /** Makes extension methods on `Ident` available without any imports */
    given IdentMethods as IdentMethods = IdentMethodsImpl

    /** Implementation of extension methods on `Ident` */
    protected val IdentMethodsImpl: IdentMethods

    /** Extension methods of `Ident` */
    trait IdentMethods:
      extension (self: Ident):
        def name: String
      end extension
    end IdentMethods

    /** Tree representing a selection of definition with a given name on a given prefix */
    type Select <: Ref

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Select` */
    given TypeTest[Tree, Select] = SelectTypeTest

    /** Implementation of `TypeTest[Tree, Select]` */
    protected val SelectTypeTest: TypeTest[Tree, Select]

    /** Module object of `type Select`  */
    val Select: SelectModule

    /** Methods of the module object `val Select` */
    trait SelectModule { this: Select.type =>
      /** Select a term member by symbol */
      def apply(qualifier: Term, symbol: Symbol): Select

      /** Select a field or a non-overloaded method by name
      *
      *  @note The method will produce an assertion error if the selected
      *        method is overloaded. The method `overloaded` should be used
      *        in that case.
      */
      def unique(qualifier: Term, name: String): Select

      /** Call an overloaded method with the given type and term parameters */
      def overloaded(qualifier: Term, name: String, targs: List[TypeRepr], args: List[Term]): Apply

      /** Call an overloaded method with the given type and term parameters */
      def overloaded(qualifier: Term, name: String, targs: List[TypeRepr], args: List[Term], returnType: TypeRepr): Apply

      def copy(original: Tree)(qualifier: Term, name: String): Select

      /** Matches `<qualifier: Term>.<name: String>` */
      def unapply(x: Select): Option[(Term, String)]
    }

    /** Makes extension methods on `Select` available without any imports */
    given SelectMethods as SelectMethods = SelectMethodsImpl

    /** Implementation of extension methods on `Select` */
    protected val SelectMethodsImpl: SelectMethods

    /** Extension methods of `Select` */
    trait SelectMethods:
      extension (self: Select):
        def qualifier: Term
        def name: String
        def signature: Option[Signature]
      end extension
    end SelectMethods

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Literal` */
    given TypeTest[Tree, Literal] = LiteralTypeTest

    /** Implementation of `TypeTest[Tree, Literal]` */
    protected val LiteralTypeTest: TypeTest[Tree, Literal]

    /** Tree representing a literal value in the source code */
    type Literal <: Term

    /** Module object of `type Literal`  */
    val Literal: LiteralModule

    /** Methods of the module object `val Literal` */
    trait LiteralModule { this: Literal.type =>

      /** Create a literal constant */
      def apply(constant: Constant): Literal

      def copy(original: Tree)(constant: Constant): Literal

      /** Matches a literal constant */
      def unapply(x: Literal): Option[Constant]
    }

    /** Makes extension methods on `Literal` available without any imports */
    given LiteralMethods as LiteralMethods = LiteralMethodsImpl

    /** Implementation of extension methods on `Literal` */
    protected val LiteralMethodsImpl: LiteralMethods

    /** Extension methods of `Literal` */
    trait LiteralMethods:
      extension (self: Literal):
        def constant: Constant
      end extension
    end LiteralMethods

    /** Tree representing `this` in the source code */
    type This <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `This` */
    given TypeTest[Tree, This] = ThisTypeTest

    /** Implementation of `TypeTest[Tree, This]` */
    protected val ThisTypeTest: TypeTest[Tree, This]

    /** Module object of `type This`  */
    val This: ThisModule

    /** Methods of the module object `val This` */
    trait ThisModule { this: This.type =>

      /** Create a `this[<id: String]>` */
      def apply(cls: Symbol): This

      def copy(original: Tree)(qual: Option[String]): This

      /** Matches `this[<id: Option[String]>` */
      def unapply(x: This): Option[Option[String]]
    }

    /** Makes extension methods on `This` available without any imports */
    given ThisMethods as ThisMethods = ThisMethodsImpl

    /** Implementation of extension methods on `This` */
    protected val ThisMethodsImpl: ThisMethods

    /** Extension methods of `This` */
    trait ThisMethods:
      extension (self: This):
        def id: Option[String]
      end extension
    end ThisMethods

    /** Tree representing `new` in the source code */
    type New <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `New` */
    given TypeTest[Tree, New] = NewTypeTest

    /** Implementation of `TypeTest[Tree, New]` */
    protected val NewTypeTest: TypeTest[Tree, New]

    /** Module object of `type New`  */
    val New: NewModule

    /** Methods of the module object `val New` */
    trait NewModule { this: New.type =>

      /** Create a `new <tpt: TypeTree>` */
      def apply(tpt: TypeTree): New

      def copy(original: Tree)(tpt: TypeTree): New

      /** Matches a `new <tpt: TypeTree>` */
      def unapply(x: New): Option[TypeTree]
    }

    /** Makes extension methods on `New` available without any imports */
    given NewMethods as NewMethods = NewMethodsImpl

    /** Implementation of extension methods on `New` */
    protected val NewMethodsImpl: NewMethods

    /** Extension methods of `New` */
    trait NewMethods:
      extension (self: New):
        def tpt: TypeTree
      end extension
    end NewMethods

    /** Tree representing an argument passed with an explicit name. Such as `arg1 = x` in `foo(arg1 = x)` */
    type NamedArg <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `NamedArg` */
    given TypeTest[Tree, NamedArg] = NamedArgTypeTest

    /** Implementation of `TypeTest[Tree, NamedArg]` */
    protected val NamedArgTypeTest: TypeTest[Tree, NamedArg]

    /** Module object of `type NamedArg`  */
    val NamedArg: NamedArgModule

    /** Methods of the module object `val NamedArg` */
    trait NamedArgModule { this: NamedArg.type =>

      /** Create a named argument `<name: String> = <value: Term>` */
      def apply(name: String, arg: Term): NamedArg

      def copy(original: Tree)(name: String, arg: Term): NamedArg

      /** Matches a named argument `<name: String> = <value: Term>` */
      def unapply(x: NamedArg): Option[(String, Term)]
    }

    /** Makes extension methods on `NamedArg` available without any imports */
    given NamedArgMethods as NamedArgMethods = NamedArgMethodsImpl

    /** Implementation of extension methods on `NamedArg` */
    protected val NamedArgMethodsImpl: NamedArgMethods

    /** Extension methods of `NamedArg` */
    trait NamedArgMethods:
      extension (self: NamedArg):
        def name: String
        def value: Term
      end extension
    end NamedArgMethods

    /** Tree an application of arguments. It represents a single list of arguments, multiple argument lists will have nested `Apply`s  */
    type Apply <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is an `Apply` */
    given TypeTest[Tree, Apply] = ApplyTypeTest

    /** Implementation of `TypeTest[Tree, Apply]` */
    protected val ApplyTypeTest: TypeTest[Tree, Apply]

    /** Module object of `type Apply`  */
    val Apply: ApplyModule

    /** Methods of the module object `val Apply` */
    trait ApplyModule { this: Apply.type =>

      /** Create a function application `<fun: Term>(<args: List[Term]>)` */
      def apply(fun: Term, args: List[Term]): Apply

      def copy(original: Tree)(fun: Term, args: List[Term]): Apply

      /** Matches a function application `<fun: Term>(<args: List[Term]>)` */
      def unapply(x: Apply): Option[(Term, List[Term])]
    }

    /** Makes extension methods on `Apply` available without any imports */
    given ApplyMethods as ApplyMethods = ApplyMethodsImpl

    /** Implementation of extension methods on `Apply` */
    protected val ApplyMethodsImpl: ApplyMethods

    /** Extension methods of `Apply` */
    trait ApplyMethods:
      extension (self: Apply):
        def fun: Term
        def args: List[Term]
      end extension
    end ApplyMethods

    /** Tree an application of type arguments */
    type TypeApply <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `TypeApply` */
    given TypeTest[Tree, TypeApply] = TypeApplyTypeTest

    /** Implementation of `TypeTest[Tree, TypeApply]` */
    protected val TypeApplyTypeTest: TypeTest[Tree, TypeApply]

    /** Module object of `type TypeApply`  */
    val TypeApply: TypeApplyModule

    /** Methods of the module object `val TypeApply` */
    trait TypeApplyModule { this: TypeApply.type =>

      /** Create a function type application `<fun: Term>[<args: List[TypeTree]>]` */
      def apply(fun: Term, args: List[TypeTree]): TypeApply

      def copy(original: Tree)(fun: Term, args: List[TypeTree]): TypeApply

      /** Matches a function type application `<fun: Term>[<args: List[TypeTree]>]` */
      def unapply(x: TypeApply): Option[(Term, List[TypeTree])]
    }

    /** Makes extension methods on `TypeApply` available without any imports */
    given TypeApplyMethods as TypeApplyMethods = TypeApplyMethodsImpl

    /** Implementation of extension methods on `TypeApply` */
    protected val TypeApplyMethodsImpl: TypeApplyMethods

    /** Extension methods of `TypeApply` */
    trait TypeApplyMethods:
      extension (self: TypeApply):
        def fun: Term
        def args: List[TypeTree]
      end extension
    end TypeApplyMethods

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Super` */
    given TypeTest[Tree, Super] = SuperTypeTest

    /** Implementation of `TypeTest[Tree, Super]` */
    protected val SuperTypeTest: TypeTest[Tree, Super]

    /** Tree representing `super` in the source code */
    type Super <: Term

    /** Module object of `type Super`  */
    val Super: SuperModule

    /** Methods of the module object `val Super` */
    trait SuperModule { this: Super.type =>

      /** Creates a `<qualifier: Term>.super[<id: Option[Id]>` */
      def apply(qual: Term, mix: Option[String]): Super

      def copy(original: Tree)(qual: Term, mix: Option[String]): Super

      /** Matches a `<qualifier: Term>.super[<id: Option[Id]>` */
      def unapply(x: Super): Option[(Term, Option[String])]
    }

    /** Makes extension methods on `Super` available without any imports */
    given SuperMethods as SuperMethods = SuperMethodsImpl

    /** Implementation of extension methods on `Super` */
    protected val SuperMethodsImpl: SuperMethods

    /** Extension methods of `Super` */
    trait SuperMethods:
      extension (self: Super):
        def qualifier: Term
        def id: Option[String]
        def idPos: Position
      end extension
    end SuperMethods

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Typed` */
    given TypeTest[Tree, Typed] = TypedTypeTest

    /** Implementation of `TypeTest[Tree, Typed]` */
    protected val TypedTypeTest: TypeTest[Tree, Typed]

    /** Tree representing a type ascription `x: T` in the source code */
    type Typed <: Term

    /** Module object of `type Typed`  */
    val Typed: TypedModule

    /** Methods of the module object `val Typed` */
    trait TypedModule { this: Typed.type =>

      /** Create a type ascription `<x: Term>: <tpt: TypeTree>` */
      def apply(expr: Term, tpt: TypeTree): Typed

      def copy(original: Tree)(expr: Term, tpt: TypeTree): Typed

      /** Matches `<expr: Term>: <tpt: TypeTree>` */
      def unapply(x: Typed): Option[(Term, TypeTree)]
    }

    /** Makes extension methods on `Typed` available without any imports */
    given TypedMethods as TypedMethods = TypedMethodsImpl

    /** Implementation of extension methods on `Typed` */
    protected val TypedMethodsImpl: TypedMethods

    /** Extension methods of `Typed` */
    trait TypedMethods:
      extension (self: Typed):
        def expr: Term
        def tpt: TypeTree
      end extension
    end TypedMethods

    /** Tree representing an assignment `x = y` in the source code */
    type Assign <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is an `Assign` */
    given TypeTest[Tree, Assign] = AssignTypeTest

    /** Implementation of `TypeTest[Tree, Assign]` */
    protected val AssignTypeTest: TypeTest[Tree, Assign]

    /** Module object of `type Assign`  */
    val Assign: AssignModule

    /** Methods of the module object `val Assign` */
    trait AssignModule { this: Assign.type =>

      /** Create an assignment `<lhs: Term> = <rhs: Term>` */
      def apply(lhs: Term, rhs: Term): Assign

      def copy(original: Tree)(lhs: Term, rhs: Term): Assign

      /** Matches an assignment `<lhs: Term> = <rhs: Term>` */
      def unapply(x: Assign): Option[(Term, Term)]
    }

    /** Makes extension methods on `Assign` available without any imports */
    given AssignMethods as AssignMethods = AssignMethodsImpl

    /** Implementation of extension methods on `Assign` */
    protected val AssignMethodsImpl: AssignMethods

    /** Extension methods of `Assign` */
    trait AssignMethods:
      extension (self: Assign):
        def lhs: Term
        def rhs: Term
      end extension
    end AssignMethods

    /** Tree representing a block `{ ... }` in the source code */
    type Block <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Block` */
    given TypeTest[Tree, Block] = BlockTypeTest

    /** Implementation of `TypeTest[Tree, Block]` */
    protected val BlockTypeTest: TypeTest[Tree, Block]

    /** Module object of `type Block`  */
    val Block: BlockModule

    /** Methods of the module object `val Block` */
    trait BlockModule { this: Block.type =>

      /** Creates a block `{ <statements: List[Statement]>; <expr: Term> }` */
      def apply(stats: List[Statement], expr: Term): Block

      def copy(original: Tree)(stats: List[Statement], expr: Term): Block

      /** Matches a block `{ <statements: List[Statement]>; <expr: Term> }` */
      def unapply(x: Block): Option[(List[Statement], Term)]
    }

    /** Makes extension methods on `Block` available without any imports */
    given BlockMethods as BlockMethods = BlockMethodsImpl

    /** Implementation of extension methods on `Block` */
    protected val BlockMethodsImpl: BlockMethods

    /** Extension methods of `Block` */
    trait BlockMethods:
      extension (self: Block):
        def statements: List[Statement]
        def expr: Term
      end extension
    end BlockMethods

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Closure` */
    given TypeTest[Tree, Closure] = ClosureTypeTest

    /** Implementation of `TypeTest[Tree, Closure]` */
    protected val ClosureTypeTest: TypeTest[Tree, Closure]

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

    /** Module object of `type Closure`  */
    val Closure: ClosureModule

    /** Methods of the module object `val Closure` */
    trait ClosureModule { this: Closure.type =>

      def apply(meth: Term, tpe: Option[TypeRepr]): Closure

      def copy(original: Tree)(meth: Tree, tpe: Option[TypeRepr]): Closure

      def unapply(x: Closure): Option[(Term, Option[TypeRepr])]
    }

    /** Makes extension methods on `Closure` available without any imports */
    given ClosureMethods as ClosureMethods = ClosureMethodsImpl

    /** Implementation of extension methods on `Closure` */
    protected val ClosureMethodsImpl: ClosureMethods

    /** Extension methods of `Closure` */
    trait ClosureMethods:
      extension (self: Closure):
        def meth: Term
        def tpeOpt: Option[TypeRepr]
      end extension
    end ClosureMethods

    /** A lambda `(...) => ...` in the source code is represented as
     *  a local method and a closure:
     *
     *  {
     *    def m(...) = ...
     *    closure(m)
     *  }
     *
     *  @note Due to the encoding, in pattern matches the case for `Lambda`
     *        should come before the case for `Block` to avoid mishandling
     *        of `Lambda`.
     */
    val Lambda: LambdaModule

    /** Methods of the module object `val Lambda` */
    trait LambdaModule { this: Lambda.type =>
      /** Matches a lambda definition of the form
       *  ```
       *  Block((DefDef(_, _, params :: Nil, _, Some(body))) :: Nil, Closure(meth, _))
       *  ```
       *  Extracts the parameter definitions and body.
       *
       */
      def unapply(tree: Block): Option[(List[ValDef], Term)]

      /** Generates a lambda with the given method type.
       *  ```
       *  Block((DefDef(_, _, params :: Nil, _, Some(rhsFn(meth, paramRefs)))) :: Nil, Closure(meth, _))
       *  ```
       * @param owner: owner of the generated `meth` symbol
       * @param tpe: Type of the definition
       * @param rhsFn: Funtion that recieves the `meth` symbol and the a list of references to the `params`
       */
      def apply(owner: Symbol, tpe: MethodType, rhsFn: (Symbol, List[Tree]) => Tree): Block
    }

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is an `If` */
    given TypeTest[Tree, If] = IfTypeTest

    /** Implementation of `TypeTest[Tree, If]` */
    protected val IfTypeTest: TypeTest[Tree, If]

    /** Tree representing an if/then/else `if (...) ... else ...` in the source code */
    type If <: Term

    /** Module object of `type If` */
    val If: IfModule

    /** Methods of the module object `val If` */
    trait IfModule { this: If.type =>

      /** Create an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>` */
      def apply(cond: Term, thenp: Term, elsep: Term): If

      def copy(original: Tree)(cond: Term, thenp: Term, elsep: Term): If

      /** Matches an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>` */
      def unapply(tree: If): Option[(Term, Term, Term)]
    }

    /** Makes extension methods on `If` available without any imports */
    given IfMethods as IfMethods = IfMethodsImpl

    /** Implementation of extension methods on `If` */
    protected val IfMethodsImpl: IfMethods

    /** Extension methods of `If` */
    trait IfMethods:
      extension (self: If):
        def cond: Term
        def thenp: Term
        def elsep: Term
      end extension
    end IfMethods

    /** Tree representing a pattern match `x match  { ... }` in the source code */
    type Match <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Match` */
    given TypeTest[Tree, Match] = MatchTypeTest

    /** Implementation of `TypeTest[Tree, Match]` */
    protected val MatchTypeTest: TypeTest[Tree, Match]

    /** Module object of `type Match`  */
    val Match: MatchModule

    /** Methods of the module object `val Match` */
    trait MatchModule { this: Match.type =>

      /** Creates a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }` */
      def apply(selector: Term, cases: List[CaseDef]): Match

      def copy(original: Tree)(selector: Term, cases: List[CaseDef]): Match

      /** Matches a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }` */
      def unapply(x: Match): Option[(Term, List[CaseDef])]
    }

    /** Makes extension methods on `Match` available without any imports */
    given MatchMethods as MatchMethods = MatchMethodsImpl

    /** Implementation of extension methods on `Match` */
    protected val MatchMethodsImpl: MatchMethods

    /** Extension methods of `Match` */
    trait MatchMethods:
      extension (self: Match):
        def scrutinee: Term
        def cases: List[CaseDef]
      end extension
    end MatchMethods

    /** Tree representing a summoning match `summonFrom { ... }` in the source code */
    type SummonFrom <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `SummonFrom` */
    given TypeTest[Tree, SummonFrom] = SummonFromTypeTest

    /** Implementation of `TypeTest[Tree, SummonFrom]` */
    protected val SummonFromTypeTest: TypeTest[Tree, SummonFrom]

    /** Module object of `type SummonFrom`  */
    val SummonFrom: SummonFromModule

    /** Methods of the module object `val SummonFrom` */
    trait SummonFromModule { this: SummonFrom.type =>

      /** Creates a pattern match `given match { <cases: List[CaseDef]> }` */
      def apply(cases: List[CaseDef]): SummonFrom

      def copy(original: Tree)(cases: List[CaseDef]): SummonFrom

      /** Matches a pattern match `given match { <cases: List[CaseDef]> }` */
      def unapply(x: SummonFrom): Option[List[CaseDef]]
    }

    /** Makes extension methods on `SummonFrom` available without any imports */
    given SummonFromMethods as SummonFromMethods = SummonFromMethodsImpl

    /** Implementation of extension methods on `SummonFrom` */
    protected val SummonFromMethodsImpl: SummonFromMethods

    /** Extension methods of `SummonFrom` */
    trait SummonFromMethods:
      extension (self: SummonFrom):
        def cases: List[CaseDef]
      end extension
    end SummonFromMethods

    /** Tree representing a try catch `try x catch { ... } finally { ... }` in the source code */
    type Try <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Try` */
    given TypeTest[Tree, Try] = TryTypeTest

    /** Implementation of `TypeTest[Tree, Try]` */
    protected val TryTypeTest: TypeTest[Tree, Try]

    /** Module object of `type Try`  */
    val Try: TryModule

    /** Methods of the module object `val Try` */
    trait TryModule { this: Try.type =>

      /** Create a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>` */
      def apply(expr: Term, cases: List[CaseDef], finalizer: Option[Term]): Try

      def copy(original: Tree)(expr: Term, cases: List[CaseDef], finalizer: Option[Term]): Try

      /** Matches a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>` */
      def unapply(x: Try): Option[(Term, List[CaseDef], Option[Term])]
    }

    /** Makes extension methods on `Try` available without any imports */
    given TryMethods as TryMethods = TryMethodsImpl

    /** Implementation of extension methods on `Try` */
    protected val TryMethodsImpl: TryMethods

    /** Extension methods of `Try` */
    trait TryMethods:
      extension (self: Try):
        def body: Term
        def cases: List[CaseDef]
        def finalizer: Option[Term]
      end extension
    end TryMethods

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Return` */
    given TypeTest[Tree, Return] = ReturnTypeTest

    /** Implementation of `TypeTest[Tree, Return]` */
    protected val ReturnTypeTest: TypeTest[Tree, Return]

    /** Tree representing a `return` in the source code */
    type Return <: Term

    /** Module object of `type Return`  */
    val Return: ReturnModule

    /** Methods of the module object `val Return` */
    trait ReturnModule { this: Return.type =>

      /** Creates `return <expr: Term>` */
      def apply(expr: Term, from: Symbol): Return

      def copy(original: Tree)(expr: Term, from: Symbol): Return

      /** Matches `return <expr: Term>` and extracts the expression and symbol of the method */
      def unapply(x: Return): Option[(Term, Symbol)]
    }

    /** Makes extension methods on `Return` available without any imports */
    given ReturnMethods as ReturnMethods = ReturnMethodsImpl

    /** Implementation of extension methods on `Return` */
    protected val ReturnMethodsImpl: ReturnMethods

    /** Extension methods of `Return` */
    trait ReturnMethods:
      extension (self: Return):
        def expr: Term
        def from: Symbol
      end extension
    end ReturnMethods

    /** Tree representing a variable argument list in the source code */
    type Repeated <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Repeated` */
    given TypeTest[Tree, Repeated] = RepeatedTypeTest

    /** Implementation of `TypeTest[Tree, Repeated]` */
    protected val RepeatedTypeTest: TypeTest[Tree, Repeated]

    /** Module object of `type Repeated`  */
    val Repeated: RepeatedModule

    /** Methods of the module object `val Repeated` */
    trait RepeatedModule { this: Repeated.type =>
      def apply(elems: List[Term], tpt: TypeTree): Repeated
      def copy(original: Tree)(elems: List[Term], tpt: TypeTree): Repeated
      def unapply(x: Repeated): Option[(List[Term], TypeTree)]
    }

    /** Makes extension methods on `Repeated` available without any imports */
    given RepeatedMethods as RepeatedMethods = RepeatedMethodsImpl

    /** Implementation of extension methods on `Repeated` */
    protected val RepeatedMethodsImpl: RepeatedMethods

    /** Extension methods of `Repeated` */
    trait RepeatedMethods:
      extension (self: Repeated):
        def elems: List[Term]
        def elemtpt: TypeTree
      end extension
    end RepeatedMethods

    /** Tree representing the scope of an inlined tree */
    type Inlined <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is an `Inlined` */
    given TypeTest[Tree, Inlined] = InlinedTypeTest

    /** Implementation of `TypeTest[Tree, Inlined]` */
    protected val InlinedTypeTest: TypeTest[Tree, Inlined]

    /** Module object of `type Inlined`  */
    val Inlined: InlinedModule

    /** Methods of the module object `val Inlined` */
    trait InlinedModule { this: Inlined.type =>
      def apply(call: Option[Tree /* Term | TypeTree */], bindings: List[Definition], expansion: Term): Inlined
      def copy(original: Tree)(call: Option[Tree /* Term | TypeTree */], bindings: List[Definition], expansion: Term): Inlined
      def unapply(x: Inlined): Option[(Option[Tree /* Term | TypeTree */], List[Definition], Term)]
    }

    /** Makes extension methods on `Inlined` available without any imports */
    given InlinedMethods as InlinedMethods = InlinedMethodsImpl

    /** Implementation of extension methods on `Inlined` */
    protected val InlinedMethodsImpl: InlinedMethods

    /** Extension methods of `Inlined` */
    trait InlinedMethods:
      extension (self: Inlined):
        def call: Option[Tree /* Term | TypeTree */]
        def bindings: List[Definition]
        def body: Term
      end extension
    end InlinedMethods

    /** Tree representing a selection of definition with a given name on a given prefix and number of nested scopes of inlined trees */
    type SelectOuter <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `SelectOuter` */
    given TypeTest[Tree, SelectOuter] = SelectOuterTypeTest

    /** Implementation of `TypeTest[Tree, SelectOuter]` */
    protected val SelectOuterTypeTest: TypeTest[Tree, SelectOuter]

    /** Module object of `type SelectOuter`  */
    val SelectOuter: SelectOuterModule

    /** Methods of the module object `val SelectOuter` */
    trait SelectOuterModule { this: SelectOuter.type =>
      def apply(qualifier: Term, name: String, levels: Int): SelectOuter
      def copy(original: Tree)(qualifier: Term, name: String, levels: Int): SelectOuter
      def unapply(x: SelectOuter): Option[(Term, String, Int)]
    }

    /** Makes extension methods on `SelectOuter` available without any imports */
    given SelectOuterMethods as SelectOuterMethods = SelectOuterMethodsImpl

    /** Implementation of extension methods on `SelectOuter` */
    protected val SelectOuterMethodsImpl: SelectOuterMethods

    /** Extension methods of `SelectOuter` */
    trait SelectOuterMethods:
      extension (self: SelectOuter):
        def qualifier: Term
        def name: String
        def level: Int
      end extension
    end SelectOuterMethods

    /** Tree representing a while loop */
    type While <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `While` */
    given TypeTest[Tree, While] = WhileTypeTest

    /** Implementation of `TypeTest[Tree, While]` */
    protected val WhileTypeTest: TypeTest[Tree, While]

    /** Module object of `type While`  */
    val While: WhileModule

    /** Methods of the module object `val While` */
    trait WhileModule { this: While.type =>

      /** Creates a while loop `while (<cond>) <body>` and returns (<cond>, <body>) */
      def apply(cond: Term, body: Term): While

      def copy(original: Tree)(cond: Term, body: Term): While

      /** Extractor for while loops. Matches `while (<cond>) <body>` and returns (<cond>, <body>) */
      def unapply(x: While): Option[(Term, Term)]
    }

    /** Makes extension methods on `While` available without any imports */
    given WhileMethods as WhileMethods = WhileMethodsImpl

    /** Implementation of extension methods on `While` */
    protected val WhileMethodsImpl: WhileMethods

    /** Extension methods of `While` */
    trait WhileMethods:
      extension (self: While):
        def cond: Term
        def body: Term
      end extension
    end WhileMethods

    // ----- TypeTrees ------------------------------------------------

    /** Type tree representing a type written in the source */
    type TypeTree <: Tree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `TypeTree` */
    given TypeTest[Tree, TypeTree] = TypeTreeTypeTest

    /** Implementation of `TypeTest[Tree, TypeTree]` */
    protected val TypeTreeTypeTest: TypeTest[Tree, TypeTree]

    /** Module object of `type TypeTree`  */
    val TypeTree: TypeTreeModule

    /** Methods of the module object `val TypeTree` */
    trait TypeTreeModule { this: TypeTree.type =>
      /** Returns the tree of type or kind (TypeTree) of T */
      def of[T <: AnyKind](using Type[T]): TypeTree
    }

    /** Makes extension methods on `TypeTree` available without any imports */
    given TypeTreeMethods as TypeTreeMethods = TypeTreeMethodsImpl

    /** Implementation of extension methods on `TypeTree` */
    protected val TypeTreeMethodsImpl: TypeTreeMethods

    /** Extension methods of `TypeTree` */
    trait TypeTreeMethods:
      extension (self: TypeTree):
        /** TypeRepr of this type tree */
        def tpe: TypeRepr
      end extension
    end TypeTreeMethods

    /** Type tree representing an inferred type */
    type Inferred <: TypeTree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is an `Inferred` */
    given TypeTest[Tree, Inferred] = InferredTypeTest

    /** Implementation of `TypeTest[Tree, Inferred]` */
    protected val InferredTypeTest: TypeTest[Tree, Inferred]

    /** Module object of `type Inferred`  */
    val Inferred: InferredModule

    /** Methods of the module object `val Inferred` */
    trait InferredModule { this: Inferred.type =>
      def apply(tpe: TypeRepr): Inferred
      /** Matches a TypeTree containing an inferred type */
      def unapply(x: Inferred): Boolean
    }

    /** Type tree representing a reference to definition with a given name */
    type TypeIdent <: TypeTree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `TypeIdent` */
    given TypeTest[Tree, TypeIdent] = TypeIdentTypeTest

    /** Implementation of `TypeTest[Tree, TypeIdent]` */
    protected val TypeIdentTypeTest: TypeTest[Tree, TypeIdent]

    /** Module object of `type TypeIdent`  */
    val TypeIdent: TypeIdentModule

    /** Methods of the module object `val TypeIdent` */
    trait TypeIdentModule { this: TypeIdent.type =>
      def apply(sym: Symbol): TypeTree
      def copy(original: Tree)(name: String): TypeIdent
      def unapply(x: TypeIdent): Option[String]
    }

    /** Makes extension methods on `TypeIdent` available without any imports */
    given TypeIdentMethods as TypeIdentMethods = TypeIdentMethodsImpl

    /** Implementation of extension methods on `TypeIdent` */
    protected val TypeIdentMethodsImpl: TypeIdentMethods

    /** Extension methods of `TypeIdent` */
    trait TypeIdentMethods:
      extension (self: TypeIdent):
        def name: String
      end extension
    end TypeIdentMethods

    /** Type tree representing a selection of definition with a given name on a given term prefix */
    type TypeSelect <: TypeTree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `TypeSelect` */
    given TypeTest[Tree, TypeSelect] = TypeSelectTypeTest

    /** Implementation of `TypeTest[Tree, TypeSelect]` */
    protected val TypeSelectTypeTest: TypeTest[Tree, TypeSelect]

    /** Module object of `type TypeSelect`  */
    val TypeSelect: TypeSelectModule

    /** Methods of the module object `val TypeSelect` */
    trait TypeSelectModule { this: TypeSelect.type =>
      def apply(qualifier: Term, name: String): TypeSelect
      def copy(original: Tree)(qualifier: Term, name: String): TypeSelect
      def unapply(x: TypeSelect): Option[(Term, String)]
    }

    /** Makes extension methods on `TypeSelect` available without any imports */
    given TypeSelectMethods as TypeSelectMethods = TypeSelectMethodsImpl

    /** Implementation of extension methods on `TypeSelect` */
    protected val TypeSelectMethodsImpl: TypeSelectMethods

    /** Extension methods of `TypeSelect` */
    trait TypeSelectMethods:
      extension (self: TypeSelect):
        def qualifier: Term
        def name: String
      end extension
    end TypeSelectMethods

    /** Type tree representing a selection of definition with a given name on a given type prefix */
    type TypeProjection <: TypeTree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `TypeProjection` */
    given TypeTest[Tree, TypeProjection] = TypeProjectionTypeTest

    /** Implementation of `TypeTest[Tree, TypeProjection]` */
    protected val TypeProjectionTypeTest: TypeTest[Tree, TypeProjection]

    /** Module object of `type TypeProjection`  */
    val TypeProjection: TypeProjectionModule

    /** Methods of the module object `val TypeProjection` */
    trait TypeProjectionModule { this: TypeProjection.type =>
      // TODO def apply(qualifier: TypeTree, name: String): Project
      def copy(original: Tree)(qualifier: TypeTree, name: String): TypeProjection
      def unapply(x: TypeProjection): Option[(TypeTree, String)]
    }

    /** Makes extension methods on `TypeProjection` available without any imports */
    given TypeProjectionMethods as TypeProjectionMethods = TypeProjectionMethodsImpl

    /** Implementation of extension methods on `TypeProjection` */
    protected val TypeProjectionMethodsImpl: TypeProjectionMethods

    /** Extension methods of `TypeProjection` */
    trait TypeProjectionMethods:
      extension (self: TypeProjection):
        def qualifier: TypeTree
        def name: String
      end extension
    end TypeProjectionMethods

    /** Type tree representing a singleton type */
    type Singleton <: TypeTree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Singleton` */
    given TypeTest[Tree, Singleton] = SingletonTypeTest

    /** Implementation of `TypeTest[Tree, Singleton]` */
    protected val SingletonTypeTest: TypeTest[Tree, Singleton]

    /** Module object of `type Singleton`  */
    val Singleton: SingletonModule

    /** Methods of the module object `val Singleton` */
    trait SingletonModule { this: Singleton.type =>
      def apply(ref: Term): Singleton
      def copy(original: Tree)(ref: Term): Singleton
      def unapply(x: Singleton): Option[Term]
    }

    /** Makes extension methods on `Singleton` available without any imports */
    given SingletonMethods as SingletonMethods = SingletonMethodsImpl

    /** Implementation of extension methods on `Singleton` */
    protected val SingletonMethodsImpl: SingletonMethods

    /** Extension methods of `Singleton` */
    trait SingletonMethods:
      extension (self: Singleton):
        def ref: Term
      end extension
    end SingletonMethods

    /** Type tree representing a type refinement */
    type Refined <: TypeTree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Refined` */
    given TypeTest[Tree, Refined] = RefinedTypeTest

    /** Implementation of `TypeTest[Tree, Refined]` */
    protected val RefinedTypeTest: TypeTest[Tree, Refined]

    /** Module object of `type Refined`  */
    val Refined: RefinedModule

    /** Methods of the module object `val Refined` */
    trait RefinedModule { this: Refined.type =>
      // TODO def apply(tpt: TypeTree, refinements: List[Definition]): Refined
      def copy(original: Tree)(tpt: TypeTree, refinements: List[Definition]): Refined
      def unapply(x: Refined): Option[(TypeTree, List[Definition])]
    }

    /** Makes extension methods on `Refined` available without any imports */
    given RefinedMethods as RefinedMethods = RefinedMethodsImpl

    /** Implementation of extension methods on `Refined` */
    protected val RefinedMethodsImpl: RefinedMethods

    /** Extension methods of `Refined` */
    trait RefinedMethods:
      extension (self: Refined):
        def tpt: TypeTree
        def refinements: List[Definition]
      end extension
    end RefinedMethods

    /** Type tree representing a type application */
    type Applied <: TypeTree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is an `Applied` */
    given TypeTest[Tree, Applied] = AppliedTypeTest

    /** Implementation of `TypeTest[Tree, Applied]` */
    protected val AppliedTypeTest: TypeTest[Tree, Applied]

    /** Module object of `type Applied`  */
    val Applied: AppliedModule

    /** Methods of the module object `val Applied` */
    trait AppliedModule { this: Applied.type =>
      def apply(tpt: TypeTree, args: List[Tree /*TypeTree | TypeBoundsTree*/]): Applied
      def copy(original: Tree)(tpt: TypeTree, args: List[Tree /*TypeTree | TypeBoundsTree*/]): Applied
      def unapply(x: Applied): Option[(TypeTree, List[Tree /*TypeTree | TypeBoundsTree*/])]
    }

    /** Makes extension methods on `Applied` available without any imports */
    given AppliedMethods as AppliedMethods = AppliedMethodsImpl

    /** Implementation of extension methods on `Applied` */
    protected val AppliedMethodsImpl: AppliedMethods

    /** Extension methods of `Applied` */
    trait AppliedMethods:
      extension (self: Applied):
        def tpt: TypeTree
        def args: List[Tree /*TypeTree | TypeBoundsTree*/]
      end extension
    end AppliedMethods

    /** Type tree representing an annotated type */
    type Annotated <: TypeTree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is an `Annotated` */
    given TypeTest[Tree, Annotated] = AnnotatedTypeTest

    /** Implementation of `TypeTest[Tree, Annotated]` */
    protected val AnnotatedTypeTest: TypeTest[Tree, Annotated]

    /** Module object of `type Annotated`  */
    val Annotated: AnnotatedModule

    /** Methods of the module object `val Annotated` */
    trait AnnotatedModule { this: Annotated.type =>
      def apply(arg: TypeTree, annotation: Term): Annotated
      def copy(original: Tree)(arg: TypeTree, annotation: Term): Annotated
      def unapply(x: Annotated): Option[(TypeTree, Term)]
    }

    /** Makes extension methods on `Annotated` available without any imports */
    given AnnotatedMethods as AnnotatedMethods = AnnotatedMethodsImpl

    /** Implementation of extension methods on `Annotated` */
    protected val AnnotatedMethodsImpl: AnnotatedMethods

    /** Extension methods of `Annotated` */
    trait AnnotatedMethods:
      extension (self: Annotated):
        def arg: TypeTree
        def annotation: Term
      end extension
    end AnnotatedMethods

    /** Type tree representing a type match */
    type MatchTypeTree <: TypeTree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `MatchTypeTree` */
    given TypeTest[Tree, MatchTypeTree] = MatchTypeTreeTypeTest

    /** Implementation of `TypeTest[Tree, MatchTypeTree]` */
    protected val MatchTypeTreeTypeTest: TypeTest[Tree, MatchTypeTree]

    /** Module object of `type MatchTypeTree`  */
    val MatchTypeTree: MatchTypeTreeModule

    /** Methods of the module object `val MatchTypeTree` */
    trait MatchTypeTreeModule { this: MatchTypeTree.type =>
      def apply(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef]): MatchTypeTree
      def copy(original: Tree)(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef]): MatchTypeTree
      def unapply(x: MatchTypeTree): Option[(Option[TypeTree], TypeTree, List[TypeCaseDef])]
    }

    /** Makes extension methods on `MatchTypeTree` available without any imports */
    given MatchTypeTreeMethods as MatchTypeTreeMethods = MatchTypeTreeMethodsImpl

    /** Implementation of extension methods on `MatchTypeTree` */
    protected val MatchTypeTreeMethodsImpl: MatchTypeTreeMethods

    /** Extension methods of `MatchTypeTree` */
    trait MatchTypeTreeMethods:
      extension (self: MatchTypeTree):
        def bound: Option[TypeTree]
        def selector: TypeTree
        def cases: List[TypeCaseDef]
      end extension
    end MatchTypeTreeMethods

    /** Type tree representing a by name parameter */
    type ByName <: TypeTree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `ByName` */
    given TypeTest[Tree, ByName] = ByNameTypeTest

    /** Implementation of `TypeTest[Tree, ByName]` */
    protected val ByNameTypeTest: TypeTest[Tree, ByName]

    /** Module object of `type ByName`  */
    val ByName: ByNameModule

    /** Methods of the module object `val ByName` */
    trait ByNameModule { this: ByName.type =>
      def apply(result: TypeTree): ByName
      def copy(original: Tree)(result: TypeTree): ByName
      def unapply(x: ByName): Option[TypeTree]
    }

    /** Makes extension methods on `ByName` available without any imports */
    given ByNameMethods as ByNameMethods = ByNameMethodsImpl

    /** Implementation of extension methods on `ByName` */
    protected val ByNameMethodsImpl: ByNameMethods

    /** Extension methods of `ByName` */
    trait ByNameMethods:
      extension (self: ByName):
        def result: TypeTree
      end extension
    end ByNameMethods

    /** Type tree representing a lambda abstraction type */
    type LambdaTypeTree <: TypeTree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `LambdaTypeTree` */
    given TypeTest[Tree, LambdaTypeTree] = LambdaTypeTreeTypeTest

    /** Implementation of `TypeTest[Tree, LambdaTypeTree]` */
    protected val LambdaTypeTreeTypeTest: TypeTest[Tree, LambdaTypeTree]

    /** Module object of `type LambdaTypeTree`  */
    val LambdaTypeTree: LambdaTypeTreeModule

    /** Methods of the module object `val LambdaTypeTree` */
    trait LambdaTypeTreeModule { this: LambdaTypeTree.type =>
      def apply(tparams: List[TypeDef], body: Tree /*TypeTree | TypeBoundsTree*/): LambdaTypeTree
      def copy(original: Tree)(tparams: List[TypeDef], body: Tree /*TypeTree | TypeBoundsTree*/): LambdaTypeTree
      def unapply(tree: LambdaTypeTree): Option[(List[TypeDef], Tree /*TypeTree | TypeBoundsTree*/)]
    }

    /** Makes extension methods on `LambdaTypeTree` available without any imports */
    given LambdaTypeTreeMethods as LambdaTypeTreeMethods = LambdaTypeTreeMethodsImpl

    /** Implementation of extension methods on `LambdaTypeTree` */
    protected val LambdaTypeTreeMethodsImpl: LambdaTypeTreeMethods

    /** Extension methods of `LambdaTypeTree` */
    trait LambdaTypeTreeMethods:
      extension (self: LambdaTypeTree):
        def tparams: List[TypeDef]
        def body: Tree /*TypeTree | TypeBoundsTree*/
      end extension
    end LambdaTypeTreeMethods

    /** Type tree representing a type binding */
    type TypeBind <: TypeTree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `TypeBind` */
    given TypeTest[Tree, TypeBind] = TypeBindTypeTest

    /** Implementation of `TypeTest[Tree, TypeBind]` */
    protected val TypeBindTypeTest: TypeTest[Tree, TypeBind]

    /** Module object of `type TypeBind`  */
    val TypeBind: TypeBindModule

    /** Methods of the module object `val TypeBind` */
    trait TypeBindModule { this: TypeBind.type =>
      // TODO def apply(name: String, tree: Tree): TypeBind
      def copy(original: Tree)(name: String, tpt: Tree /*TypeTree | TypeBoundsTree*/): TypeBind
      def unapply(x: TypeBind): Option[(String, Tree /*TypeTree | TypeBoundsTree*/)]
    }

    /** Makes extension methods on `TypeBind` available without any imports */
    given TypeBindMethods as TypeBindMethods = TypeBindMethodsImpl

    /** Implementation of extension methods on `TypeBind` */
    protected val TypeBindMethodsImpl: TypeBindMethods

    /** Extension methods of `TypeBind` */
    trait TypeBindMethods:
      extension (self: TypeBind):
        def name: String
        def body: Tree /*TypeTree | TypeBoundsTree*/
      end extension
    end TypeBindMethods

    /** Type tree within a block with aliases `{ type U1 = ... ; T[U1, U2] }` */
    type TypeBlock <: TypeTree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `TypeBlock` */
    given TypeTest[Tree, TypeBlock] = TypeBlockTypeTest

    /** Implementation of `TypeTest[Tree, TypeBlock]` */
    protected val TypeBlockTypeTest: TypeTest[Tree, TypeBlock]

    /** Module object of `type TypeBlock`  */
    val TypeBlock: TypeBlockModule

    /** Methods of the module object `val TypeBlock` */
    trait TypeBlockModule { this: TypeBlock.type =>
      def apply(aliases: List[TypeDef], tpt: TypeTree): TypeBlock
      def copy(original: Tree)(aliases: List[TypeDef], tpt: TypeTree): TypeBlock
      def unapply(x: TypeBlock): Option[(List[TypeDef], TypeTree)]
    }

    /** Makes extension methods on `TypeBlock` available without any imports */
    given TypeBlockMethods as TypeBlockMethods = TypeBlockMethodsImpl

    /** Implementation of extension methods on `TypeBlock` */
    protected val TypeBlockMethodsImpl: TypeBlockMethods

    /** Extension methods of `TypeBlock` */
    trait TypeBlockMethods:
      extension (self: TypeBlock):
        def aliases: List[TypeDef]
        def tpt: TypeTree
      end extension
    end TypeBlockMethods

    // ----- TypeBoundsTrees ------------------------------------------------

    /** Type tree representing a type bound written in the source */
    type TypeBoundsTree <: Tree /*TypeTree | TypeBoundsTree*/

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `TypeBoundsTree` */
    given TypeTest[Tree, TypeBoundsTree] = TypeBoundsTreeTypeTest

    /** Implementation of `TypeTest[Tree, TypeBoundsTree]` */
    protected val TypeBoundsTreeTypeTest: TypeTest[Tree, TypeBoundsTree]

    /** Module object of `type TypeBoundsTree`  */
    val TypeBoundsTree: TypeBoundsTreeModule

    /** Methods of the module object `val TypeBoundsTree` */
    trait TypeBoundsTreeModule { this: TypeBoundsTree.type =>
      def apply(low: TypeTree, hi: TypeTree): TypeBoundsTree
      def copy(original: Tree)(low: TypeTree, hi: TypeTree): TypeBoundsTree
      def unapply(x: TypeBoundsTree): Option[(TypeTree, TypeTree)]
    }

    /** Makes extension methods on `TypeBoundsTree` available without any imports */
    given TypeBoundsTreeMethods as TypeBoundsTreeMethods = TypeBoundsTreeMethodsImpl

    /** Implementation of extension methods on `TypeBoundsTree` */
    protected val TypeBoundsTreeMethodsImpl: TypeBoundsTreeMethods

    /** Extension methods of `TypeBoundsTree` */
    trait TypeBoundsTreeMethods:
      extension (self: TypeBoundsTree):
        def tpe: TypeBounds
        def low: TypeTree
        def hi: TypeTree
      end extension
    end TypeBoundsTreeMethods

    /** Type tree representing wildcard type bounds written in the source.
    *  The wildcard type `_` (for example in in `List[_]`) will be a type tree that
    *  represents a type but has `TypeBound`a inside.
    */
    type WildcardTypeTree  <: Tree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `WildcardTypeTree` */
    given TypeTest[Tree, WildcardTypeTree] = WildcardTypeTreeTypeTest

    /** Implementation of `TypeTest[Tree, WildcardTypeTree]` */
    protected val WildcardTypeTreeTypeTest: TypeTest[Tree, WildcardTypeTree]

    /** Module object of `type WildcardTypeTree`  */
    val WildcardTypeTree: WildcardTypeTreeModule

    /** Methods of the module object `val WildcardTypeTree` */
    trait WildcardTypeTreeModule { this: WildcardTypeTree.type =>
      def apply(tpe: TypeRepr): WildcardTypeTree
      /** Matches a TypeBoundsTree containing wildcard type bounds */
      def unapply(x: WildcardTypeTree): Boolean
    }

    /** Makes extension methods on `WildcardTypeTree` available without any imports */
    given WildcardTypeTreeMethods as WildcardTypeTreeMethods = WildcardTypeTreeMethodsImpl

    /** Implementation of extension methods on `WildcardTypeTree` */
    protected val WildcardTypeTreeMethodsImpl: WildcardTypeTreeMethods

    /** Extension methods of `WildcardTypeTree` */
    trait WildcardTypeTreeMethods:
      extension (self: WildcardTypeTree):
        def tpe: TypeRepr
      end extension
    end WildcardTypeTreeMethods

    // ----- CaseDefs ------------------------------------------------

    /** Branch of a pattern match or catch clause */
    type CaseDef <: Tree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `CaseDef` */
    given TypeTest[Tree, CaseDef] = CaseDefTypeTest

    /** Implementation of `TypeTest[Tree, CaseDef]` */
    protected val CaseDefTypeTest: TypeTest[Tree, CaseDef]

    /** Module object of `type CaseDef`  */
    val CaseDef: CaseDefModule

    /** Methods of the module object `val CaseDef` */
    trait CaseDefModule { this: CaseDef.type =>
      def apply(pattern: Tree, guard: Option[Term], rhs: Term): CaseDef
      def copy(original: Tree)(pattern: Tree, guard: Option[Term], rhs: Term): CaseDef
      def unapply(x: CaseDef): Option[(Tree, Option[Term], Term)]
    }

    /** Makes extension methods on `CaseDef` available without any imports */
    given CaseDefMethods as CaseDefMethods = CaseDefMethodsImpl

    /** Implementation of extension methods on `CaseDef` */
    protected val CaseDefMethodsImpl: CaseDefMethods

    /** Extension methods of `CaseDef` */
    trait CaseDefMethods:
      extension (self: CaseDef):
        def pattern: Tree
        def guard: Option[Term]
        def rhs: Term
      end extension
    end CaseDefMethods

    /** Branch of a type pattern match */
    type TypeCaseDef <: Tree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `TypeCaseDef` */
    given TypeTest[Tree, TypeCaseDef] = TypeCaseDefTypeTest

    /** Implementation of `TypeTest[Tree, TypeCaseDef]` */
    protected val TypeCaseDefTypeTest: TypeTest[Tree, TypeCaseDef]

    /** Module object of `type TypeCaseDef`  */
    val TypeCaseDef: TypeCaseDefModule

    /** Methods of the module object `val TypeCaseDef` */
    trait TypeCaseDefModule { this: TypeCaseDef.type =>
      def apply(pattern: TypeTree, rhs: TypeTree): TypeCaseDef
      def copy(original: Tree)(pattern: TypeTree, rhs: TypeTree): TypeCaseDef
      def unapply(tree: TypeCaseDef): Option[(TypeTree, TypeTree)]
    }

    /** Makes extension methods on `TypeCaseDef` available without any imports */
    given TypeCaseDefMethods as TypeCaseDefMethods = TypeCaseDefMethodsImpl

    /** Implementation of extension methods on `TypeCaseDef` */
    protected val TypeCaseDefMethodsImpl: TypeCaseDefMethods

    /** Extension methods of `TypeCaseDef` */
    trait TypeCaseDefMethods:
      extension (self: TypeCaseDef):
        def pattern: TypeTree
        def rhs: TypeTree
      end extension
    end TypeCaseDefMethods

    // ----- Patterns ------------------------------------------------

    /** Pattern representing a `_ @ _` binding. */
    type Bind <: Tree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Bind` */
    given TypeTest[Tree, Bind] = BindTypeTest

    /** Implementation of `TypeTest[Tree, Bind]` */
    protected val BindTypeTest: TypeTest[Tree, Bind]

    /** Module object of `type Bind`  */
    val Bind: BindModule

    /** Methods of the module object `val Bind` */
    trait BindModule { this: Bind.type =>
      def apply(sym: Symbol, pattern: Tree): Bind
      def copy(original: Tree)(name: String, pattern: Tree): Bind
      def unapply(pattern: Bind): Option[(String, Tree)]
    }

    /** Makes extension methods on `Bind` available without any imports */
    given BindMethods as BindMethods = BindMethodsImpl

    /** Implementation of extension methods on `Bind` */
    protected val BindMethodsImpl: BindMethods

    /** Extension methods of `Bind` */
    trait BindMethods:
      extension (self: Bind):
        def name: String
        def pattern: Tree
      end extension
    end BindMethods

    /** Pattern representing a `Xyz(...)` unapply. */
    type Unapply <: Tree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is an `Unapply` */
    given TypeTest[Tree, Unapply] = UnapplyTypeTest

    /** Implementation of `TypeTest[Tree, Unapply]` */
    protected val UnapplyTypeTest: TypeTest[Tree, Unapply]

    /** Module object of `type Unapply`  */
    val Unapply: UnapplyModule

    /** Methods of the module object `val Unapply` */
    trait UnapplyModule { this: Unapply.type =>
      // TODO def apply(fun: Term, implicits: List[Term], patterns: List[Tree]): Unapply
      def copy(original: Tree)(fun: Term, implicits: List[Term], patterns: List[Tree]): Unapply
      def unapply(x: Unapply): Option[(Term, List[Term], List[Tree])]
    }

    /** Makes extension methods on `Unapply` available without any imports */
    given UnapplyMethods as UnapplyMethods = UnapplyMethodsImpl

    /** Implementation of extension methods on `Unapply` */
    protected val UnapplyMethodsImpl: UnapplyMethods

    /** Extension methods of `Unapply` */
    trait UnapplyMethods:
      extension (self: Unapply):
        def fun: Term
        def implicits: List[Term]
        def patterns: List[Tree]
      end extension
    end UnapplyMethods

    /** Pattern representing `X | Y | ...` alternatives. */
    type Alternatives <: Tree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is an `Alternatives` */
    given TypeTest[Tree, Alternatives] = AlternativesTypeTest

    /** Implementation of `TypeTest[Tree, Alternatives]` */
    protected val AlternativesTypeTest: TypeTest[Tree, Alternatives]

    /** Module object of `type Alternatives`  */
    val Alternatives: AlternativesModule

    /** Methods of the module object `val Alternatives` */
    trait AlternativesModule { this: Alternatives.type =>
      def apply(patterns: List[Tree]): Alternatives
      def copy(original: Tree)(patterns: List[Tree]): Alternatives
      def unapply(x: Alternatives): Option[List[Tree]]
    }

    /** Makes extension methods on `Alternatives` available without any imports */
    given AlternativesMethods as AlternativesMethods = AlternativesMethodsImpl

    /** Implementation of extension methods on `Alternatives` */
    protected val AlternativesMethodsImpl: AlternativesMethods

    /** Extension methods of `Alternatives` */
    trait AlternativesMethods:
      extension (self: Alternatives):
        def patterns: List[Tree]
      end extension
    end AlternativesMethods

    //////////////////////
    // IMPORT SELECTORS //
    /////////////////////

    /** Import selectors:
    *   * SimpleSelector: `.bar` in `import foo.bar`
    *   * RenameSelector: `.{bar => baz}` in `import foo.{bar => baz}`
    *   * OmitSelector: `.{bar => _}` in `import foo.{bar => _}`
    *   * GivneSelector: `.given`/`.{given T}` in `import foo.given`/`import foo.{given T}`
    */
    type ImportSelector <: AnyRef

    /** Module object of `type ImportSelector`  */
    val ImportSelector: ImportSelectorModule

    /** Methods of the module object `val ImportSelector` */
    trait ImportSelectorModule { this: ImportSelector.type => }

    /** Simple import selector: `.bar` in `import foo.bar` */
    type SimpleSelector <: ImportSelector

    /** `TypeTest` that allows testing at runtime in a pattern match if an `ImportSelector` is a `SimpleSelector` */
    given TypeTest[ImportSelector, SimpleSelector] = SimpleSelectorTypeTest

    /** Implementation of `TypeTest[ImportSelector, SimpleSelector]` */
    protected val SimpleSelectorTypeTest: TypeTest[ImportSelector, SimpleSelector]

    /** Module object of `type SimpleSelector`  */
    val SimpleSelector: SimpleSelectorModule

    /** Methods of the module object `val SimpleSelector` */
    trait SimpleSelectorModule { this: SimpleSelector.type =>
      def unapply(x: SimpleSelector): Option[String]
    }

    /** Makes extension methods on `SimpleSelector` available without any imports */
    given SimpleSelectorMethods as SimpleSelectorMethods = SimpleSelectorMethodsImpl

    /** Implementation of extension methods on `SimpleSelector` */
    protected val SimpleSelectorMethodsImpl: SimpleSelectorMethods

    /** Extension methods of `SimpleSelector` */
    trait SimpleSelectorMethods:
      extension (self: SimpleSelector):
        def name: String
        def namePos: Position
      end extension
    end SimpleSelectorMethods

    /** Rename import selector: `.{bar => baz}` in `import foo.{bar => baz}` */
    type RenameSelector <: ImportSelector

    /** `TypeTest` that allows testing at runtime in a pattern match if an `ImportSelector` is a `RenameSelector` */
    given TypeTest[ImportSelector, RenameSelector] = RenameSelectorTypeTest

    /** Implementation of `TypeTest[ImportSelector, RenameSelector]` */
    protected val RenameSelectorTypeTest: TypeTest[ImportSelector, RenameSelector]

    /** Module object of `type RenameSelector`  */
    val RenameSelector: RenameSelectorModule

    /** Methods of the module object `val RenameSelector` */
    trait RenameSelectorModule { this: RenameSelector.type =>
      def unapply(x: RenameSelector): Option[(String, String)]
    }

    /** Makes extension methods on `RenameSelector` available without any imports */
    given RenameSelectorMethods as RenameSelectorMethods = RenameSelectorMethodsImpl

    /** Implementation of extension methods on `RenameSelector` */
    protected val RenameSelectorMethodsImpl: RenameSelectorMethods

    /** Extension methods of `RenameSelector` */
    trait RenameSelectorMethods:
      extension (self: RenameSelector):
        def fromName: String
        def fromPos: Position
        def toName: String
        def toPos: Position
      end extension
    end RenameSelectorMethods

    /** Omit import selector: `.{bar => _}` in `import foo.{bar => _}` */
    type OmitSelector <: ImportSelector

    /** `TypeTest` that allows testing at runtime in a pattern match if an `ImportSelector` is an `OmitSelector` */
    given TypeTest[ImportSelector, OmitSelector] = OmitSelectorTypeTest

    /** Implementation of `TypeTest[ImportSelector, OmitSelector]` */
    protected val OmitSelectorTypeTest: TypeTest[ImportSelector, OmitSelector]

    /** Module object of `type OmitSelector`  */
    val OmitSelector: OmitSelectorModule

    /** Methods of the module object `val OmitSelector` */
    trait OmitSelectorModule { this: OmitSelector.type =>
      def unapply(x: OmitSelector): Option[String]
    }

    /** Makes extension methods on `OmitSelector` available without any imports */
    given OmitSelectorMethods as OmitSelectorMethods = OmitSelectorMethodsImpl

    /** Implementation of extension methods on `OmitSelector` */
    protected val OmitSelectorMethodsImpl: OmitSelectorMethods

    /** Extension methods of `OmitSelector` */
    trait OmitSelectorMethods:
      extension (self: OmitSelector):
        def name: String
        def namePos: Position
    end OmitSelectorMethods

    /** Omit import selector: `.given`/`.{given T}` in `import foo.given`/`import foo.{given T}` */
    type GivenSelector <: ImportSelector

    /** `TypeTest` that allows testing at runtime in a pattern match if an `ImportSelector` is a `GivenSelector` */
    given TypeTest[ImportSelector, GivenSelector] = GivenSelectorTypeTest

    /** Implementation of `TypeTest[ImportSelector, GivenSelector]` */
    protected val GivenSelectorTypeTest: TypeTest[ImportSelector, GivenSelector]

    /** Module object of `type GivenSelector`  */
    val GivenSelector: GivenSelectorModule

    /** Methods of the module object `val GivenSelector` */
    trait GivenSelectorModule { this: GivenSelector.type =>
      def unapply(x: GivenSelector): Option[Option[TypeTree]]
    }

    /** Makes extension methods on `GivenSelector` available without any imports */
    given GivenSelectorMethods as GivenSelectorMethods = GivenSelectorMethodsImpl

    /** Implementation of extension methods on `GivenSelector` */
    protected val GivenSelectorMethodsImpl: GivenSelectorMethods

    /** Extension methods of `GivenSelector` */
    trait GivenSelectorMethods:
      extension (self: GivenSelector):
        def bound: Option[TypeTree]
    end GivenSelectorMethods

    ///////////////
    //   TYPES   //
    ///////////////

    // ----- Types ----------------------------------------------------

    /** A type, type constructors, type bounds or NoPrefix */
    type TypeRepr

    /** Module object of `type TypeRepr`  */
    val TypeRepr: TypeReprModule

    /** Methods of the module object `val TypeRepr` */
    trait TypeReprModule { this: TypeRepr.type =>
      /** Returns the type or kind (TypeRepr) of T */
      def of[T <: AnyKind](using Type[T]): TypeRepr

      /** Returns the type constructor of the runtime (erased) class */
      def typeConstructorOf(clazz: Class[?]): TypeRepr
    }

    /** Makes extension methods on `TypeRepr` available without any imports */
    given TypeReprMethods as TypeReprMethods = TypeReprMethodsImpl

    /** Implementation of extension methods on `TypeRepr` */
    protected val TypeReprMethodsImpl: TypeReprMethods

    /** Extension methods of `TypeRepr` */
    trait TypeReprMethods {
      extension (self: TypeRepr):

        /** Shows the tree as extractors */
        def showExtractors: String

        /** Shows the tree as fully typed source code */
        def show: String

        /** Shows the tree as fully typed source code colored with ANSI */
        def showAnsiColored: String

        /** Convert this `TypeRepr` to an `Type[?]`
        *
        *  Usage:
        *  ```
        *  typeRepr.asType match
        *    case '[$t] =>
        *      '{ val x: t = ... }
        *  ```
        */
        def asType: Type[?]

        /** Is `self` type the same as `that` type?
        *  This is the case iff `self <:< that` and `that <:< self`.
        */
        def =:=(that: TypeRepr): Boolean

        /** Is this type a subtype of that type? */
        def <:<(that: TypeRepr): Boolean

        /** Widen from singleton type to its underlying non-singleton
        *  base type by applying one or more `underlying` dereferences,
        *  Also go from => T to T.
        *  Identity for all other types. Example:
        *
        *  class Outer { class C ; val x: C }
        *  def o: Outer
        *  <o.x.type>.widen = o.C
        */
        def widen: TypeRepr

        /** Widen from TermRef to its underlying non-termref
          *  base type, while also skipping `=>T` types.
          */
        def widenTermRefExpr: TypeRepr

        /** Follow aliases and dereferences LazyRefs, annotated types and instantiated
          *  TypeVars until type is no longer alias type, annotated type, LazyRef,
          *  or instantiated type variable.
          */
        def dealias: TypeRepr

        /** A simplified version of this type which is equivalent wrt =:= to this type.
        *  Reduces typerefs, applied match types, and and or types.
        */
        def simplified: TypeRepr

        def classSymbol: Option[Symbol]
        def typeSymbol: Symbol
        def termSymbol: Symbol
        def isSingleton: Boolean
        def memberType(member: Symbol): TypeRepr

        /** The base classes of this type with the class itself as first element. */
        def baseClasses: List[Symbol]

        /** The least type instance of given class which is a super-type
        *  of this type.  Example:
        *  {{{
        *    class D[T]
        *    class C extends p.D[Int]
        *    ThisType(C).baseType(D) = p.D[Int]
        * }}}
        */
        def baseType(cls: Symbol): TypeRepr

        /** Is this type an instance of a non-bottom subclass of the given class `cls`? */
        def derivesFrom(cls: Symbol): Boolean

        /** Is this type a function type?
        *
        *  @return true if the dealised type of `self` without refinement is `FunctionN[T1, T2, ..., Tn]`
        *
        *  @note The function
        *
        *     - returns true for `given Int => Int` and `erased Int => Int`
        *     - returns false for `List[Int]`, despite that `List[Int] <:< Int => Int`.
        */
        def isFunctionType: Boolean

        /** Is this type an context function type?
        *
        *  @see `isFunctionType`
        */
        def isContextFunctionType: Boolean

        /** Is this type an erased function type?
        *
        *  @see `isFunctionType`
        */
        def isErasedFunctionType: Boolean

        /** Is this type a dependent function type?
        *
        *  @see `isFunctionType`
        */
        def isDependentFunctionType: Boolean

        /** The type <this . sym>, reduced if possible */
        def select(sym: Symbol): TypeRepr

        /** The current type applied to given type arguments: `this[targ]` */
        def appliedTo(targ: TypeRepr): TypeRepr

        /** The current type applied to given type arguments: `this[targ0, ..., targN]` */
        def appliedTo(targs: List[TypeRepr]): TypeRepr

      end extension
    }

    /** A singleton type representing a known constant value */
    type ConstantType <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `ConstantType` */
    given TypeTest[TypeRepr, ConstantType] = ConstantTypeTypeTest

    /** Implementation of `TypeTest[TypeRepr, ConstantType]` */
    protected val ConstantTypeTypeTest: TypeTest[TypeRepr, ConstantType]

    /** Module object of `type ConstantType`  */
    val ConstantType: ConstantTypeModule

    /** Methods of the module object `val Type` */
    trait ConstantTypeModule { this: ConstantType.type =>
      def apply(x : Constant): ConstantType
      def unapply(x: ConstantType): Option[Constant]
    }

    /** Makes extension methods on `ConstantType` available without any imports */
    given ConstantTypeMethods as ConstantTypeMethods = ConstantTypeMethodsImpl

    /** Implementation of extension methods on `ConstantType` */
    protected val ConstantTypeMethodsImpl: ConstantTypeMethods

    /** Extension methods of `ConstantType` */
    trait ConstantTypeMethods:
      extension (self: ConstantType):
        def constant: Constant
      end extension
    end ConstantTypeMethods

    /** Type of a reference to a term symbol */
    type TermRef <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `TermRef` */
    given TypeTest[TypeRepr, TermRef] = TermRefTypeTest

    /** Implementation of `TypeTest[TypeRepr, TermRef]` */
    protected val TermRefTypeTest: TypeTest[TypeRepr, TermRef]

    /** Module object of `type TermRef`  */
    val TermRef: TermRefModule

    /** Methods of the module object `val TermRef` */
    trait TermRefModule { this: TermRef.type =>
      def apply(qual: TypeRepr, name: String): TermRef
      def unapply(x: TermRef): Option[(TypeRepr, String)]
    }

    /** Makes extension methods on `TermRef` available without any imports */
    given TermRefMethods as TermRefMethods = TermRefMethodsImpl

    /** Implementation of extension methods on `TermRef` */
    protected val TermRefMethodsImpl: TermRefMethods

    /** Extension methods of `TermRef` */
    trait TermRefMethods:
      extension (self: TermRef):
        def qualifier: TypeRepr
        def name: String
      end extension
    end TermRefMethods

    /** Type of a reference to a type symbol */
    type TypeRef <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `TypeRef` */
    given TypeTest[TypeRepr, TypeRef] = TypeRefTypeTest

    /** Implementation of `TypeTest[TypeRepr, TypeRef]` */
    protected val TypeRefTypeTest: TypeTest[TypeRepr, TypeRef]

    /** Module object of `type TypeRef`  */
    val TypeRef: TypeRefModule

    /** Methods of the module object `val TypeRef` */
    trait TypeRefModule { this: TypeRef.type =>
      def unapply(x: TypeRef): Option[(TypeRepr, String)]
    }

    /** Makes extension methods on `TypeRef` available without any imports */
    given TypeRefMethods as TypeRefMethods = TypeRefMethodsImpl

    /** Implementation of extension methods on `TypeRef` */
    protected val TypeRefMethodsImpl: TypeRefMethods

    /** Extension methods of `TypeRef` */
    trait TypeRefMethods:
      extension (self: TypeRef):
        def qualifier: TypeRepr
        def name: String
        def isOpaqueAlias: Boolean
        def translucentSuperType: TypeRepr
      end extension
    end TypeRefMethods

    /** Type of a `super` reference */
    type SuperType <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `SuperType` */
    given TypeTest[TypeRepr, SuperType] = SuperTypeTypeTest

    /** Implementation of `TypeTest[TypeRepr, SuperType]` */
    protected val SuperTypeTypeTest: TypeTest[TypeRepr, SuperType]

    /** Module object of `type SuperType`  */
    val SuperType: SuperTypeModule

    /** Methods of the module object `val SuperType` */
    trait SuperTypeModule { this: SuperType.type =>
      def apply(thistpe: TypeRepr, supertpe: TypeRepr): SuperType
      def unapply(x: SuperType): Option[(TypeRepr, TypeRepr)]
    }

    /** Makes extension methods on `SuperType` available without any imports */
    given SuperTypeMethods as SuperTypeMethods = SuperTypeMethodsImpl

    /** Implementation of extension methods on `SuperType` */
    protected val SuperTypeMethodsImpl: SuperTypeMethods

    /** Extension methods of `SuperType` */
    trait SuperTypeMethods { this: SuperTypeMethods =>
      extension (self: SuperType):
        def thistpe: TypeRepr
        def supertpe: TypeRepr
      end extension
    }

    /** A type with a type refinement `T { type U }` */
    type Refinement <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `Refinement` */
    given TypeTest[TypeRepr, Refinement] = RefinementTypeTest

    /** Implementation of `TypeTest[TypeRepr, Refinement]` */
    protected val RefinementTypeTest: TypeTest[TypeRepr, Refinement]

    /** Module object of `type Refinement`  */
    val Refinement: RefinementModule

    /** Methods of the module object `val Refinement` */
    trait RefinementModule { this: Refinement.type =>
      def apply(parent: TypeRepr, name: String, info: TypeRepr): Refinement
      def unapply(x: Refinement): Option[(TypeRepr, String, TypeRepr)]
    }

    /** Makes extension methods on `Refinement` available without any imports */
    given RefinementMethods as RefinementMethods = RefinementMethodsImpl

    /** Implementation of extension methods on `Refinement` */
    protected val RefinementMethodsImpl: RefinementMethods

    /** Extension methods of `Refinement` */
    trait RefinementMethods:
      extension (self: Refinement):
        def parent: TypeRepr
        def name: String
        def info: TypeRepr
      end extension
    end RefinementMethods

    /** A higher kinded type applied to some types `T[U]` */
    type AppliedType <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is an `AppliedType` */
    given TypeTest[TypeRepr, AppliedType] = AppliedTypeTypeTest

    /** Implementation of `TypeTest[TypeRepr, AppliedType]` */
    protected val AppliedTypeTypeTest: TypeTest[TypeRepr, AppliedType]

    /** Module object of `type AppliedType`  */
    val AppliedType: AppliedTypeModule

    /** Methods of the module object `val AppliedType` */
    trait AppliedTypeModule { this: AppliedType.type =>
      def unapply(x: AppliedType): Option[(TypeRepr, List[TypeRepr])]
    }

    /** Makes extension methods on `AppliedType` available without any imports */
    given AppliedTypeMethods as AppliedTypeMethods = AppliedTypeMethodsImpl

    /** Implementation of extension methods on `AppliedType` */
    protected val AppliedTypeMethodsImpl: AppliedTypeMethods

    /** Extension methods of `AppliedType` */
    trait AppliedTypeMethods:
      extension (self: AppliedType):
        def tycon: TypeRepr
        def args: List[TypeRepr]
      end extension
    end AppliedTypeMethods

    /** A type with an anottation `T @foo` */
    type AnnotatedType <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is an `AnnotatedType` */
    given TypeTest[TypeRepr, AnnotatedType] = AnnotatedTypeTypeTest

    /** Implementation of `TypeTest[TypeRepr, AnnotatedType]` */
    protected val AnnotatedTypeTypeTest: TypeTest[TypeRepr, AnnotatedType]

    /** Module object of `type AnnotatedType`  */
    val AnnotatedType: AnnotatedTypeModule

    /** Methods of the module object `val AnnotatedType` */
    trait AnnotatedTypeModule { this: AnnotatedType.type =>
      def apply(underlying: TypeRepr, annot: Term): AnnotatedType
      def unapply(x: AnnotatedType): Option[(TypeRepr, Term)]
    }

    /** Makes extension methods on `AnnotatedType` available without any imports */
    given AnnotatedTypeMethods as AnnotatedTypeMethods = AnnotatedTypeMethodsImpl

    /** Implementation of extension methods on `AnnotatedType` */
    protected val AnnotatedTypeMethodsImpl: AnnotatedTypeMethods

    /** Extension methods of `AnnotatedType` */
    trait AnnotatedTypeMethods:
      extension (self: AnnotatedType):
        def underlying: TypeRepr
        def annot: Term
      end extension
    end AnnotatedTypeMethods

    /** Intersection type `T & U` */
    type AndType <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is an `AndType` */
    given TypeTest[TypeRepr, AndType] = AndTypeTypeTest

    /** Implementation of `TypeTest[TypeRepr, AndType]` */
    protected val AndTypeTypeTest: TypeTest[TypeRepr, AndType]

    /** Module object of `type AndType`  */
    val AndType: AndTypeModule

    /** Methods of the module object `val AndType` */
    trait AndTypeModule { this: AndType.type =>
      def apply(lhs: TypeRepr, rhs: TypeRepr): AndType
      def unapply(x: AndType): Option[(TypeRepr, TypeRepr)]
    }

    /** Makes extension methods on `AndType` available without any imports */
    given AndTypeMethods as AndTypeMethods = AndTypeMethodsImpl

    /** Implementation of extension methods on `AndType` */
    protected val AndTypeMethodsImpl: AndTypeMethods

    /** Extension methods of `AndType` */
    trait AndTypeMethods:
      extension (self: AndType):
        def left: TypeRepr
        def right: TypeRepr
      end extension
    end AndTypeMethods

    /** Union type `T | U` */
    type OrType <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is an `OrType` */
    given TypeTest[TypeRepr, OrType] = OrTypeTypeTest

    /** Implementation of `TypeTest[TypeRepr, OrType]` */
    protected val OrTypeTypeTest: TypeTest[TypeRepr, OrType]

    /** Module object of `type OrType`  */
    val OrType: OrTypeModule

    /** Methods of the module object `val OrType` */
    trait OrTypeModule { this: OrType.type =>
      def apply(lhs: TypeRepr, rhs: TypeRepr): OrType
      def unapply(x: OrType): Option[(TypeRepr, TypeRepr)]
    }

    /** Makes extension methods on `OrType` available without any imports */
    given OrTypeMethods as OrTypeMethods = OrTypeMethodsImpl

    /** Implementation of extension methods on `OrType` */
    protected val OrTypeMethodsImpl: OrTypeMethods

    /** Extension methods of `OrType` */
    trait OrTypeMethods:
      extension (self: OrType):
        def left: TypeRepr
        def right: TypeRepr
      end extension
    end OrTypeMethods

    /** Type match `T match { case U => ... }` */
    type MatchType <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `MatchType` */
    given TypeTest[TypeRepr, MatchType] = MatchTypeTypeTest

    /** Implementation of `TypeTest[TypeRepr, MatchType]` */
    protected val MatchTypeTypeTest: TypeTest[TypeRepr, MatchType]

    /** Module object of `type MatchType`  */
    val MatchType: MatchTypeModule

    /** Methods of the module object `val MatchType` */
    trait MatchTypeModule { this: MatchType.type =>
      def apply(bound: TypeRepr, scrutinee: TypeRepr, cases: List[TypeRepr]): MatchType
      def unapply(x: MatchType): Option[(TypeRepr, TypeRepr, List[TypeRepr])]
    }

    /** Makes extension methods on `MatchType` available without any imports */
    given MatchTypeMethods as MatchTypeMethods = MatchTypeMethodsImpl

    /** Implementation of extension methods on `MatchType` */
    protected val MatchTypeMethodsImpl: MatchTypeMethods

    /** Extension methods of `MatchType` */
    trait MatchTypeMethods:
      extension (self: MatchType):
        def bound: TypeRepr
        def scrutinee: TypeRepr
        def cases: List[TypeRepr]
      end extension
    end MatchTypeMethods

    /** Type of a by by name parameter */
    type ByNameType <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `ByNameType` */
    given TypeTest[TypeRepr, ByNameType] = ByNameTypeTypeTest

    /** Implementation of `TypeTest[TypeRepr, ByNameType]` */
    protected val ByNameTypeTypeTest: TypeTest[TypeRepr, ByNameType]

    /** Module object of `type ByNameType`  */
    val ByNameType: ByNameTypeModule

    /** Methods of the module object `val ByNameType` */
    trait ByNameTypeModule { this: ByNameType.type =>
      def apply(underlying: TypeRepr): TypeRepr
      def unapply(x: ByNameType): Option[TypeRepr]
    }

    /** Makes extension methods on `ByNameType` available without any imports */
    given ByNameTypeMethods as ByNameTypeMethods = ByNameTypeMethodsImpl

    /** Implementation of extension methods on `ByNameType` */
    protected val ByNameTypeMethodsImpl: ByNameTypeMethods

    /** Extension methods of `ByNameType` */
    trait ByNameTypeMethods:
      extension (self: ByNameType):
        def underlying: TypeRepr
      end extension
    end ByNameTypeMethods

    /** Type of a parameter reference */
    type ParamRef <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `ParamRef` */
    given TypeTest[TypeRepr, ParamRef] = ParamRefTypeTest

    /** Implementation of `TypeTest[TypeRepr, ParamRef]` */
    protected val ParamRefTypeTest: TypeTest[TypeRepr, ParamRef]

    /** Module object of `type ParamRef`  */
    val ParamRef: ParamRefModule

    /** Methods of the module object `val ParamRef` */
    trait ParamRefModule { this: ParamRef.type =>
      def unapply(x: ParamRef): Option[(LambdaType, Int)]
    }

    /** Makes extension methods on `ParamRef` available without any imports */
    given ParamRefMethods as ParamRefMethods = ParamRefMethodsImpl

    /** Implementation of extension methods on `ParamRef` */
    protected val ParamRefMethodsImpl: ParamRefMethods

    /** Extension methods of `ParamRef` */
    trait ParamRefMethods:
      extension (self: ParamRef):
        def binder: LambdaType
        def paramNum: Int
      end extension
    end ParamRefMethods

    /** Type of `this` */
    type ThisType <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `ThisType` */
    given TypeTest[TypeRepr, ThisType] = ThisTypeTypeTest

    /** Implementation of `TypeTest[TypeRepr, ThisType]` */
    protected val ThisTypeTypeTest: TypeTest[TypeRepr, ThisType]

    /** Module object of `type ThisType`  */
    val ThisType: ThisTypeModule

    /** Methods of the module object `val ThisType` */
    trait ThisTypeModule { this: ThisType.type =>
      def unapply(x: ThisType): Option[TypeRepr]
    }

    /** Makes extension methods on `ThisType` available without any imports */
    given ThisTypeMethods as ThisTypeMethods = ThisTypeMethodsImpl

    /** Implementation of extension methods on `ThisType` */
    protected val ThisTypeMethodsImpl: ThisTypeMethods

    /** Extension methods of `ThisType` */
    trait ThisTypeMethods:
      extension (self: ThisType):
        def tref: TypeRepr
      end extension
    end ThisTypeMethods

    /** A type that is recursively defined `this` */
    type RecursiveThis <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `RecursiveThis` */
    given TypeTest[TypeRepr, RecursiveThis] = RecursiveThisTypeTest

    /** Implementation of `TypeTest[TypeRepr, RecursiveThis]` */
    protected val RecursiveThisTypeTest: TypeTest[TypeRepr, RecursiveThis]

    /** Module object of `type RecursiveThis`  */
    val RecursiveThis: RecursiveThisModule

    /** Methods of the module object `val RecursiveThis` */
    trait RecursiveThisModule { this: RecursiveThis.type =>
      def unapply(x: RecursiveThis): Option[RecursiveType]
    }

    /** Makes extension methods on `RecursiveThis` available without any imports */
    given RecursiveThisMethods as RecursiveThisMethods = RecursiveThisMethodsImpl

    /** Implementation of extension methods on `RecursiveThis` */
    protected val RecursiveThisMethodsImpl: RecursiveThisMethods

    /** Extension methods of `RecursiveThis` */
    trait RecursiveThisMethods:
      extension (self: RecursiveThis):
        def binder: RecursiveType
      end extension
    end RecursiveThisMethods

    /** A type that is recursively defined */
    type RecursiveType <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `RecursiveType` */
    given TypeTest[TypeRepr, RecursiveType] = RecursiveTypeTypeTest

    /** Implementation of `TypeTest[TypeRepr, RecursiveType]` */
    protected val RecursiveTypeTypeTest: TypeTest[TypeRepr, RecursiveType]

    /** Module object of `type RecursiveType`  */
    val RecursiveType: RecursiveTypeModule

    /** Methods of the module object `val RecursiveType` */
    trait RecursiveTypeModule { this: RecursiveType.type =>

      /** Create a RecType, normalizing its contents. This means:
      *
      *   1. Nested Rec types on the type's spine are merged with the outer one.
      *   2. Any refinement of the form `type T = z.T` on the spine of the type
      *      where `z` refers to the created rec-type is replaced by
      *      `type T`. This avoids infinite recursions later when we
      *      try to follow these references.
      */
      def apply(parentExp: RecursiveType => TypeRepr): RecursiveType

      def unapply(x: RecursiveType): Option[TypeRepr]
    }

    /** Makes extension methods on `RecursiveType` available without any imports */
    given RecursiveTypeMethods as RecursiveTypeMethods = RecursiveTypeMethodsImpl

    /** Implementation of extension methods on `RecursiveType` */
    protected val RecursiveTypeMethodsImpl: RecursiveTypeMethods

    /** Extension methods of `RecursiveType` */
    trait RecursiveTypeMethods:
      extension (self: RecursiveType):
        def underlying: TypeRepr
        def recThis: RecursiveThis
      end extension
    end RecursiveTypeMethods

    // TODO: remove LambdaType and use union types (MethodType | PolyType | TypeLambda)
    /** Common abstraction for lambda types (MethodType, PolyType and TypeLambda). */
    type LambdaType <: TypeRepr

    /** Type of the definition of a method taking a single list of parameters. It's return type may be a MethodType. */
    type MethodType <: LambdaType

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `MethodType` */
    given TypeTest[TypeRepr, MethodType] = MethodTypeTypeTest

    /** Implementation of `TypeTest[TypeRepr, MethodType]` */
    protected val MethodTypeTypeTest: TypeTest[TypeRepr, MethodType]

    /** Module object of `type MethodType`  */
    val MethodType: MethodTypeModule

    /** Methods of the module object `val MethodType` */
    trait MethodTypeModule { this: MethodType.type =>
      def apply(paramNames: List[String])(paramInfosExp: MethodType => List[TypeRepr], resultTypeExp: MethodType => TypeRepr): MethodType
      def unapply(x: MethodType): Option[(List[String], List[TypeRepr], TypeRepr)]
    }

    /** Makes extension methods on `MethodType` available without any imports */
    given MethodTypeMethods as MethodTypeMethods = MethodTypeMethodsImpl

    /** Implementation of extension methods on `MethodType` */
    protected val MethodTypeMethodsImpl: MethodTypeMethods

    /** Extension methods of `MethodType` */
    trait MethodTypeMethods:
      extension (self: MethodType):
        def isImplicit: Boolean
        def isErased: Boolean
        def param(idx: Int): TypeRepr
        def paramNames: List[String]
        def paramTypes: List[TypeRepr]
        def resType: TypeRepr
      end extension
    end MethodTypeMethods

    /** Type of the definition of a method taking a list of type parameters. It's return type may be a MethodType. */
    type PolyType <: LambdaType

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `PolyType` */
    given TypeTest[TypeRepr, PolyType] = PolyTypeTypeTest

    /** Implementation of `TypeTest[TypeRepr, PolyType]` */
    protected val PolyTypeTypeTest: TypeTest[TypeRepr, PolyType]

    /** Module object of `type PolyType`  */
    val PolyType: PolyTypeModule

    /** Methods of the module object `val PolyType` */
    trait PolyTypeModule { this: PolyType.type =>
      def apply(paramNames: List[String])(paramBoundsExp: PolyType => List[TypeBounds], resultTypeExp: PolyType => TypeRepr): PolyType
      def unapply(x: PolyType): Option[(List[String], List[TypeBounds], TypeRepr)]
    }

    /** Makes extension methods on `PolyType` available without any imports */
    given PolyTypeMethods as PolyTypeMethods = PolyTypeMethodsImpl

    /** Implementation of extension methods on `PolyType` */
    protected val PolyTypeMethodsImpl: PolyTypeMethods

    /** Extension methods of `PolyType` */
    trait PolyTypeMethods:
      extension (self: PolyType):
        def param(idx: Int): TypeRepr
        def paramNames: List[String]
        def paramBounds: List[TypeBounds]
        def resType: TypeRepr
      end extension
    end PolyTypeMethods

    /** Type of the definition of a type lambda taking a list of type parameters. It's return type may be a TypeLambda. */
    type TypeLambda <: LambdaType

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `TypeLambda` */
    given TypeTest[TypeRepr, TypeLambda] = TypeLambdaTypeTest

    /** Implementation of `TypeTest[TypeRepr, TypeLambda]` */
    protected val TypeLambdaTypeTest: TypeTest[TypeRepr, TypeLambda]

    /** Module object of `type TypeLambda`  */
    val TypeLambda: TypeLambdaModule

    /** Methods of the module object `val TypeLambda` */
    trait TypeLambdaModule { this: TypeLambda.type =>
      def apply(paramNames: List[String], boundsFn: TypeLambda => List[TypeBounds], bodyFn: TypeLambda => TypeRepr): TypeLambda
      def unapply(x: TypeLambda): Option[(List[String], List[TypeBounds], TypeRepr)]
    }

    /** Makes extension methods on `TypeLambda` available without any imports */
    given TypeLambdaMethods as TypeLambdaMethods = TypeLambdaMethodsImpl

    /** Implementation of extension methods on `TypeLambda` */
    protected val TypeLambdaMethodsImpl: TypeLambdaMethods

    /** Extension methods of `TypeLambda` */
    trait TypeLambdaMethods:
      extension (self: TypeLambda):
        def paramNames: List[String]
        def paramBounds: List[TypeBounds]
        def param(idx: Int) : TypeRepr
        def resType: TypeRepr
      end extension
    end TypeLambdaMethods

    // ----- TypeBounds -----------------------------------------------

    /** Type bounds */
    type TypeBounds <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `TypeBounds` */
    given TypeTest[TypeRepr, TypeBounds] = TypeBoundsTypeTest

    /** Implementation of `TypeTest[TypeRepr, TypeBounds]` */
    protected val TypeBoundsTypeTest: TypeTest[TypeRepr, TypeBounds]

    /** Module object of `type TypeBounds`  */
    val TypeBounds: TypeBoundsModule

    /** Methods of the module object `val TypeBounds` */
    trait TypeBoundsModule { this: TypeBounds.type =>
      def apply(low: TypeRepr, hi: TypeRepr): TypeBounds
      def unapply(x: TypeBounds): Option[(TypeRepr, TypeRepr)]
      def empty: TypeBounds
      def upper(hi: TypeRepr): TypeBounds
      def lower(lo: TypeRepr): TypeBounds
    }

    /** Makes extension methods on `TypeBounds` available without any imports */
    given TypeBoundsMethods as TypeBoundsMethods = TypeBoundsMethodsImpl

    /** Implementation of extension methods on `TypeBounds` */
    protected val TypeBoundsMethodsImpl: TypeBoundsMethods

    /** Extension methods of `TypeBounds` */
    trait TypeBoundsMethods:
      extension (self: TypeBounds):
        def low: TypeRepr
        def hi: TypeRepr
      end extension
    end TypeBoundsMethods

    // ----- NoPrefix -------------------------------------------------

    /** NoPrefix for a type selection */
    type NoPrefix <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `NoPrefix` */
    given TypeTest[TypeRepr, NoPrefix] = NoPrefixTypeTest

    /** Implementation of `TypeTest[TypeRepr, NoPrefix]` */
    protected val NoPrefixTypeTest: TypeTest[TypeRepr, NoPrefix]

    /** Module object of `type NoPrefix`  */
    val NoPrefix: NoPrefixModule

    /** Methods of the module object `val NoPrefix` */
    trait NoPrefixModule { this: NoPrefix.type =>
      def unapply(x: NoPrefix): Boolean
    }

    ///////////////
    // CONSTANTS //
    ///////////////

    /** Constant value represented as the constant itself */
    type Constant <: AnyRef

    /** Constant value represented as the constant itself
     *
     * Usage:
     * ```
     *   Constant.Int(3) match
     *     case Constant.Int(n) =>
     * ```
     */
    val Constant: ConstantModule

    /** Constant value represented as the constant itself */
    trait ConstantModule { this: Constant.type =>

      /** Constant Boolean value */
      val Boolean: BooleanModule

      /** Methods of the module object `val Boolean` */
      trait BooleanModule { this: Boolean.type =>
        /** Create a constant Boolean value */
        def apply(x: Boolean): Constant
        /** Match Boolean value constant and extract its value */
        def unapply(constant: Constant): Option[Boolean]
      }

      /** Constant Byte value */
      val Byte: ByteModule

      /** Methods of the module object `val Byte` */
      trait ByteModule { this: Byte.type =>
        /** Create a constant Byte value */
        def apply(x: Byte): Constant
        /** Match Byte value constant and extract its value */
        def unapply(constant: Constant): Option[Byte]
      }

      /** Constant Short value */
      val Short: ShortModule

      /** Methods of the module object `val Short` */
      trait ShortModule { this: Short.type =>
        /** Create a constant Short value */
        def apply(x: Short): Constant
        /** Match Short value constant and extract its value */
        def unapply(constant: Constant): Option[Short]
      }

      /** Constant Int value */
      val Int: IntModule

      /** Methods of the module object `val Int` */
      trait IntModule { this: Int.type =>
        /** Create a constant Int value */
        def apply(x: Int): Constant
        /** Match Int value constant and extract its value */
        def unapply(constant: Constant): Option[Int]
      }

      /** Constant Long value */
      val Long: LongModule

      /** Methods of the module object `val Long` */
      trait LongModule { this: Long.type =>
        /** Create a constant Long value */
        def apply(x: Long): Constant
        /** Match Long value constant and extract its value */
        def unapply(constant: Constant): Option[Long]
      }

      /** Constant Float value */
      val Float: FloatModule

      /** Methods of the module object `val Float` */
      trait FloatModule { this: Float.type =>
        /** Create a constant Float value */
        def apply(x: Float): Constant
        /** Match Float value constant and extract its value */
        def unapply(constant: Constant): Option[Float]
      }

      /** Constant Double value */
      val Double: DoubleModule

      /** Methods of the module object `val Double` */
      trait DoubleModule { this: Double.type =>
        /** Create a constant Double value */
        def apply(x: Double): Constant
        /** Match Double value constant and extract its value */
        def unapply(constant: Constant): Option[Double]
      }

      /** Constant Char value */
      val Char: CharModule

      /** Methods of the module object `val Char` */
      trait CharModule { this: Char.type =>
        /** Create a constant Char value */
        def apply(x: Char): Constant
        /** Match Char value constant and extract its value */
        def unapply(constant: Constant): Option[Char]
      }

      /** Constant String value */
      val String: StringModule

      /** Methods of the module object `val String` */
      trait StringModule { this: String.type =>
        /** Create a constant String value */
        def apply(x: String): Constant
        /** Match String value constant and extract its value */
        def unapply(constant: Constant): Option[String]
      }

      /** Constant Unit value */
      val Unit: UnitModule

      /** Methods of the module object `val Unit` */
      trait UnitModule { this: Unit.type =>
        /** Create a constant Unit value */
        def apply(): Constant
        /** Match Unit value constant */
        def unapply(constant: Constant): Boolean
      }

      /** Constant null value */
      val Null: NullModule

      /** Methods of the module object `val Null` */
      trait NullModule { this: Null.type =>
        /** Create a constant null value */
        def apply(): Constant
        /** Match null value constant */
        def unapply(constant: Constant): Boolean
      }

      /** Constant class value representing a `classOf[T]` */
      val ClassOf: ClassOfModule

      /** Methods of the module object `val ClassOf` */
      trait ClassOfModule { this: ClassOf.type =>
        /** Create a constant class value representing `classOf[<tpe>]` */
        def apply(tpe: TypeRepr): Constant
        /** Match a class value constant representing `classOf[<tpe>]` and extract its type */
        def unapply(constant: Constant): Option[TypeRepr]
      }

    }

    /** Makes extension methods on `Constant` available without any imports */
    given ConstantMethods as ConstantMethods = ConstantMethodsImpl

    /** Implementation of extension methods on `Constant` */
    protected val ConstantMethodsImpl: ConstantMethods

    /** Extension methods of `Constant` */
    trait ConstantMethods {
      extension (self: Constant):
        /** Returns the value of the constant */
        def value: Any

        /** Shows the tree as extractors */
        def showExtractors: String

        /** Shows the tree as fully typed source code */
        def show: String

        /** Shows the tree as fully typed source code colored with ANSI */
        def showAnsiColored: String
      end extension
    }

    /////////////////////
    // IMPLICIT SEARCH //
    /////////////////////

    /** Module object of `type Implicits`  */
    val Implicits: ImplicitsModule

    /** Methods of the module object `val Implicits` */
    trait ImplicitsModule { self: Implicits.type =>
      /** Find a given instance of type `T` in the current scope provided by the current enclosing splice.
      *  Return an `ImplicitSearchResult`.
      *
      *  @param tpe type of the implicit parameter
      */
      def search(tpe: TypeRepr): ImplicitSearchResult
    }

    /** Result of a given instance search */
    type ImplicitSearchResult <: AnyRef

    /** `TypeTest` that allows testing at runtime in a pattern match if an `ImplicitSearchResult` is an `ImplicitSearchSuccess` */
    given TypeTest[ImplicitSearchResult, ImplicitSearchSuccess] = ImplicitSearchSuccessTypeTest

    /** Implementation of `TypeTest[ImplicitSearchResult, ImplicitSearchSuccess]` */
    protected val ImplicitSearchSuccessTypeTest: TypeTest[ImplicitSearchResult, ImplicitSearchSuccess]

    type ImplicitSearchSuccess <: ImplicitSearchResult

    /** Makes extension methods on `ImplicitSearchSuccess` available without any imports */
    given ImplicitSearchSuccessMethods as ImplicitSearchSuccessMethods = ImplicitSearchSuccessMethodsImpl

    /** Implementation of extension methods on `ImplicitSearchSuccess` */
    protected val ImplicitSearchSuccessMethodsImpl: ImplicitSearchSuccessMethods

    /** Extension methods of `ImplicitSearchSuccess` */
    trait ImplicitSearchSuccessMethods:
      extension (self: ImplicitSearchSuccess):
        def tree: Term
      end extension
    end ImplicitSearchSuccessMethods

    type ImplicitSearchFailure <: ImplicitSearchResult

    /** `TypeTest` that allows testing at runtime in a pattern match if an `ImplicitSearchResult` is an `ImplicitSearchFailure` */
    given TypeTest[ImplicitSearchResult, ImplicitSearchFailure] = ImplicitSearchFailureTypeTest

    /** Implementation of `TypeTest[ImplicitSearchResult, ImplicitSearchFailure]` */
    protected val ImplicitSearchFailureTypeTest: TypeTest[ImplicitSearchResult, ImplicitSearchFailure]

    /** Makes extension methods on `ImplicitSearchFailure` available without any imports */
    given ImplicitSearchFailureMethods as ImplicitSearchFailureMethods = ImplicitSearchFailureMethodsImpl

    /** Implementation of extension methods on `ImplicitSearchFailure` */
    protected val ImplicitSearchFailureMethodsImpl: ImplicitSearchFailureMethods

    /** Extension methods of `ImplicitSearchFailure` */
    trait ImplicitSearchFailureMethods:
      extension (self: ImplicitSearchFailure):
        def explanation: String
      end extension
    end ImplicitSearchFailureMethods

    type DivergingImplicit <: ImplicitSearchFailure

    /** `TypeTest` that allows testing at runtime in a pattern match if an `ImplicitSearchResult` is a `DivergingImplicit` */
    given TypeTest[ImplicitSearchResult, DivergingImplicit] = DivergingImplicitTypeTest

    /** Implementation of `TypeTest[ImplicitSearchResult, DivergingImplicit]` */
    protected val DivergingImplicitTypeTest: TypeTest[ImplicitSearchResult, DivergingImplicit]

    type NoMatchingImplicits <: ImplicitSearchFailure

    /** `TypeTest` that allows testing at runtime in a pattern match if an `ImplicitSearchResult` is a `NoMatchingImplicits` */
    given TypeTest[ImplicitSearchResult, NoMatchingImplicits] = NoMatchingImplicitsTypeTest

    /** Implementation of `TypeTest[ImplicitSearchResult, NoMatchingImplicits]` */
    protected val NoMatchingImplicitsTypeTest: TypeTest[ImplicitSearchResult, NoMatchingImplicits]

    type AmbiguousImplicits <: ImplicitSearchFailure

    /** `TypeTest` that allows testing at runtime in a pattern match if an `ImplicitSearchResult` is an `AmbiguousImplicits` */
    given TypeTest[ImplicitSearchResult, AmbiguousImplicits] = AmbiguousImplicitsTypeTest

    /** Implementation of `TypeTest[ImplicitSearchResult, AmbiguousImplicits]` */
    protected val AmbiguousImplicitsTypeTest: TypeTest[ImplicitSearchResult, AmbiguousImplicits]

    /////////////
    // SYMBOLS //
    /////////////

    /** Symbol of a definition.
    *  Then can be compared with == to know if the definition is the same.
    */
    type Symbol <: AnyRef

    /** Module object of `type Symbol`  */
    val Symbol: SymbolModule

    /** Methods of the module object `val Symbol` */
    trait SymbolModule { this: Symbol.type =>

      /** Symbol of the definition that encloses the current splicing context.
       *
       *  For example, the following call to `spliceOwner` would return the symbol `x`.
       *  ```
       *  val x = ${ ... Symbol.spliceOwner ... }
       *  ```
       *
       *   For a macro splice, it is the symbol of the definition where the macro expansion happens.
       */
      def spliceOwner: Symbol

      /** Get package symbol if package is either defined in current compilation run or present on classpath. */
      def requiredPackage(path: String): Symbol

      /** Get class symbol if class is either defined in current compilation run or present on classpath. */
      def requiredClass(path: String): Symbol

      /** Get module symbol if module is either defined in current compilation run or present on classpath. */
      def requiredModule(path: String): Symbol

      /** Get method symbol if method is either defined in current compilation run or present on classpath. Throws if the method has an overload. */
      def requiredMethod(path: String): Symbol

      /** The class Symbol of a global class definition */
      def classSymbol(fullName: String): Symbol

      /** Generates a new method symbol with the given parent, name and type.
      *
      *  This symbol starts without an accompanying definition.
      *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
      *  this symbol to the DefDef constructor.
      *
      *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
      *        direct or indirect children of the reflection context's owner.
      */
      def newMethod(parent: Symbol, name: String, tpe: TypeRepr): Symbol

      /** Works as the other newMethod, but with additional parameters.
      *
      *  @param flags extra flags to with which the symbol should be constructed
      *  @param privateWithin the symbol within which this new method symbol should be private. May be noSymbol.
      */
      def newMethod(parent: Symbol, name: String, tpe: TypeRepr, flags: Flags, privateWithin: Symbol): Symbol

      /** Generates a new val/var/lazy val symbol with the given parent, name and type.
      *
      *  This symbol starts without an accompanying definition.
      *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
      *  this symbol to the ValDef constructor.
      *
      *  Note: Also see Reflection.let
      *
      *  @param flags extra flags to with which the symbol should be constructed
      *  @param privateWithin the symbol within which this new method symbol should be private. May be noSymbol.
      *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
      *        direct or indirect children of the reflection context's owner.
      */
      def newVal(parent: Symbol, name: String, tpe: TypeRepr, flags: Flags, privateWithin: Symbol): Symbol

      /** Generates a pattern bind symbol with the given parent, name and type.
      *
      *  This symbol starts without an accompanying definition.
      *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
      *  this symbol to the BindDef constructor.
      *
      *  @param flags extra flags to with which the symbol should be constructed
      *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
      *        direct or indirect children of the reflection context's owner.
      */
      def newBind(parent: Symbol, name: String, flags: Flags, tpe: TypeRepr): Symbol

      /** Definition not available */
      def noSymbol: Symbol
    }

    /** Makes extension methods on `Symbol` available without any imports */
    given SymbolMethods as SymbolMethods = SymbolMethodsImpl

    /** Implementation of extension methods on `Symbol` */
    protected val SymbolMethodsImpl: SymbolMethods

    /** Extension methods of `Symbol` */
    trait SymbolMethods {
      extension (self: Symbol):

        /** Owner of this symbol. The owner is the symbol in which this symbol is defined. Throws if this symbol does not have an owner. */
        def owner: Symbol

        /** Owner of this symbol. The owner is the symbol in which this symbol is defined. Returns `NoSymbol` if this symbol does not have an owner. */
        def maybeOwner: Symbol

        /** Flags of this symbol */
        def flags: Flags

        /** This symbol is private within the resulting type */
        def privateWithin: Option[TypeRepr]

        /** This symbol is protected within the resulting type */
        def protectedWithin: Option[TypeRepr]

        /** The name of this symbol */
        def name: String

        /** The full name of this symbol up to the root package */
        def fullName: String

        /** The position of this symbol */
        def pos: Position

        /** The documentation for this symbol, if any */
        def documentation: Option[Documentation]

        /** Tree of this definition
        *
        *  If this symbol `isClassDef` it will return `a `ClassDef`,
        *  if this symbol `isTypeDef` it will return `a `TypeDef`,
        *  if this symbol `isValDef` it will return `a `ValDef`,
        *  if this symbol `isDefDef` it will return `a `DefDef`
        *  if this symbol `isBind` it will return `a `Bind`,
        *  else will throw
        */
        def tree: Tree

        /** Annotations attached to this symbol */
        def annots: List[Term]

        /** Does this symbol come from a currently compiled source file? */
        def isDefinedInCurrentRun: Boolean

        /** Dummy val symbol that owns all statements within the initialization of the class.
        *  This may also contain local definitions such as classes defined in a `locally` block in the class.
        */
        def isLocalDummy: Boolean

        /** Is this symbol a class representing a refinement? */
        def isRefinementClass: Boolean

        /** Is this symbol an alias type? */
        def isAliasType: Boolean

        /** Is this symbol an anonymous class? */
        def isAnonymousClass: Boolean

        /** Is this symbol an anonymous function? */
        def isAnonymousFunction: Boolean

        /** Is this symbol an abstract type? */
        def isAbstractType: Boolean

        /** Is this the constructor of a class? */
        def isClassConstructor: Boolean

        /** Is this the definition of a type? */
        def isType: Boolean

        /** Is this the definition of a term? */
        def isTerm: Boolean

        /** Is this the definition of a PackageDef tree? */
        def isPackageDef: Boolean

        /** Is this the definition of a ClassDef tree? */
        def isClassDef: Boolean

        /** Is this the definition of a TypeDef tree */
        def isTypeDef: Boolean

        /** Is this the definition of a ValDef tree? */
        def isValDef: Boolean

        /** Is this the definition of a DefDef tree? */
        def isDefDef: Boolean

        /** Is this the definition of a Bind pattern? */
        def isBind: Boolean

        /** Does this symbol represent a no definition? */
        def isNoSymbol: Boolean

        /** Does this symbol represent a definition? */
        def exists: Boolean

        /** Fields directly declared in the class */
        def fields: List[Symbol]

        /** Field with the given name directly declared in the class */
        def field(name: String): Symbol

        /** Get non-private named methods defined directly inside the class */
        def classMethod(name: String): List[Symbol]

        /** Get all non-private methods defined directly inside the class, exluding constructors */
        def classMethods: List[Symbol]

        /** Type member directly declared in the class */
        def typeMembers: List[Symbol]

        /** Type member with the given name directly declared in the class */
        def typeMember(name: String): Symbol

        /** All members directly declared in the class */
        def members: List[Symbol]

        /** Get named non-private methods declared or inherited */
        def method(name: String): List[Symbol]

        /** Get all non-private methods declared or inherited */
        def methods: List[Symbol]

        /** The symbols of each type parameter list and value parameter list of this
          *  method, or Nil if this isn't a method.
          */
        def paramSymss: List[List[Symbol]]

        /** The primary constructor of a class or trait, `noSymbol` if not applicable. */
        def primaryConstructor: Symbol

        /** Fields of a case class type -- only the ones declared in primary constructor */
        def caseFields: List[Symbol]

        def isTypeParam: Boolean

        /** Signature of this definition */
        def signature: Signature

        /** The class symbol of the companion module class */
        def moduleClass: Symbol

        /** The symbol of the companion class */
        def companionClass: Symbol

        /** The symbol of the companion module */
        def companionModule: Symbol

        /** Shows the tree as extractors */
        def showExtractors: String

        /** Shows the tree as fully typed source code */
        def show: String

        /** Shows the tree as fully typed source code colored with ANSI */
        def showAnsiColored: String

        /** Case class or case object children of a sealed trait */
        def children: List[Symbol]
      end extension
    }

    ////////////////
    // SIGNATURES //
    ////////////////

    /** The signature of a method */
    type Signature <: AnyRef

    /** Module object of `type Signature`  */
    val Signature: SignatureModule

    /** Methods of the module object `val Signature` */
    trait SignatureModule { this: Signature.type =>
      /** Matches the method signature and returns its parameters and result type. */
      def unapply(sig: Signature): Option[(List[String | Int], String)]
    }

    /** Makes extension methods on `Signature` available without any imports */
    given SignatureMethods as SignatureMethods = SignatureMethodsImpl

    /** Implementation of extension methods on `Signature` */
    protected val SignatureMethodsImpl: SignatureMethods

    /** Extension methods of `Signature` */
    trait SignatureMethods {
      extension (self: Signature):

        /** The signatures of the method parameters.
          *
          *  Each *type parameter section* is represented by a single Int corresponding
          *  to the number of type parameters in the section.
          *  Each *term parameter* is represented by a String corresponding to the fully qualified
          *  name of the parameter type.
          */
        def paramSigs: List[String | Int]

        /** The signature of the result type */
        def resultSig: String

      end extension
    }

    //////////////////////////
    // STANDARD DEFINITIONS //
    //////////////////////////

    /** A value containing all standard definitions */
    val defn: defnModule

    /** Methods of the module object `val defn` */
    trait defnModule { self: defn.type =>

      /** The module symbol of root package `_root_`. */
      def RootPackage: Symbol

      /** The class symbol of root package `_root_`. */
      def RootClass: Symbol

      /** The class symbol of empty package `_root_._empty_`. */
      def EmptyPackageClass: Symbol

      /** The module symbol of package `scala`. */
      def ScalaPackage: Symbol

      /** The class symbol of package `scala`. */
      def ScalaPackageClass: Symbol

      /** The class symbol of core class `scala.Any`. */
      def AnyClass: Symbol

      /** The class symbol of core class `scala.AnyVal`. */
      def AnyValClass: Symbol

      /** The class symbol of core class `java.lang.Object`. */
      def ObjectClass: Symbol

      /** The type symbol of core class `scala.AnyRef`. */
      def AnyRefClass: Symbol

      /** The class symbol of core class `scala.Null`. */
      def NullClass: Symbol

      /** The class symbol of core class `scala.Nothing`. */
      def NothingClass: Symbol

      /** The class symbol of primitive class `scala.Unit`. */
      def UnitClass: Symbol

      /** The class symbol of primitive class `scala.Byte`. */
      def ByteClass: Symbol

      /** The class symbol of primitive class `scala.Short`. */
      def ShortClass: Symbol

      /** The class symbol of primitive class `scala.Char`. */
      def CharClass: Symbol

      /** The class symbol of primitive class `scala.Int`. */
      def IntClass: Symbol

      /** The class symbol of primitive class `scala.Long`. */
      def LongClass: Symbol

      /** The class symbol of primitive class `scala.Float`. */
      def FloatClass: Symbol

      /** The class symbol of primitive class `scala.Double`. */
      def DoubleClass: Symbol

      /** The class symbol of primitive class `scala.Boolean`. */
      def BooleanClass: Symbol

      /** The class symbol of class `scala.String`. */
      def StringClass: Symbol

      /** The class symbol of class `java.lang.Class`. */
      def ClassClass: Symbol

      /** The class symbol of class `scala.Array`. */
      def ArrayClass: Symbol

      /** The module symbol of module `scala.Predef`. */
      def PredefModule: Symbol

      /** The method symbol of method `scala.Predef.classOf`. */
      def Predef_classOf: Symbol

      /** The module symbol of package `java.lang`. */
      def JavaLangPackage: Symbol

      /** The module symbol of module `scala.Array`. */
      def ArrayModule: Symbol

      /** The method symbol of method `apply` in class `scala.Array`. */
      def Array_apply: Symbol

      /** The method symbol of method `clone` in class `scala.Array`. */
      def Array_clone: Symbol

      /** The method symbol of method `length` in class `scala.Array`. */
      def Array_length: Symbol

      /** The method symbol of method `update` in class `scala.Array`. */
      def Array_update: Symbol

      /** A dummy class symbol that is used to indicate repeated parameters
      *  compiled by the Scala compiler.
      */
      def RepeatedParamClass: Symbol

      /** The class symbol of class `scala.annotation.reflection.Repeated` */
      def RepeatedAnnot: Symbol

      /** The class symbol of class `scala.Option`. */
      def OptionClass: Symbol

      /** The module symbol of module `scala.None`. */
      def NoneModule: Symbol

      /** The module symbol of module `scala.Some`. */
      def SomeModule: Symbol

      /** Function-like object that maps arity to symbols for classes `scala.Product` */
      def ProductClass: Symbol

      /** Function-like object that maps arity to symbols for classes `scala.FunctionX`.
      *   -  0th element is `Function0`
      *   -  1st element is `Function1`
      *   -  ...
      *   -  Nth element is `FunctionN`
      */
      def FunctionClass(arity: Int, isImplicit: Boolean = false, isErased: Boolean = false): Symbol

      /** Function-like object that maps arity to symbols for classes `scala.TupleX`.
      *   -  0th element is `NoSymbol`
      *   -  1st element is `NoSymbol`
      *   -  2st element is `Tuple2`
      *   -  ...
      *   - 22nd element is `Tuple22`
      *   - 23nd element is `NoSymbol`  // TODO update when we will have more tuples
      *   - ...
      */
      def TupleClass(arity: Int): Symbol

      /** Returns `true` if `sym` is a `Tuple1`, `Tuple2`, ... `Tuple22` */
      def isTupleClass(sym: Symbol): Boolean

      /** Contains Scala primitive value classes:
      *   - Byte
      *   - Short
      *   - Int
      *   - Long
      *   - Float
      *   - Double
      *   - Char
      *   - Boolean
      *   - Unit
      */
      def ScalaPrimitiveValueClasses: List[Symbol]

      /** Contains Scala numeric value classes:
      *   - Byte
      *   - Short
      *   - Int
      *   - Long
      *   - Float
      *   - Double
      *   - Char
      */
      def ScalaNumericValueClasses: List[Symbol]

    }

    ///////////////
    //   FLAGS   //
    ///////////////

    /** FlagSet of a Symbol */
    type Flags

    /** Module object of `type Flags`  */
    val Flags: FlagsModule

    /** Methods of the module object `val Flags` */
    trait FlagsModule { this: Flags.type =>

      /** Is this symbol `abstract` */
      def Abstract: Flags

      /** Was this symbol generated by Scala compiler */
      def Artifact: Flags

      /** Is this symbol `case` */
      def Case: Flags

      /** Is this symbol a getter for case class parameter */
      def CaseAccessor: Flags

      /** Is this symbol a type parameter marked as contravariant `-` */
      def Contravariant: Flags

      /** Is this symbol a type parameter marked as covariant `+` */
      def Covariant: Flags

      /** The empty set of flags */
      def EmptyFlags: Flags

      /** Is this symbol an enum */
      def Enum: Flags

      /** Is this symbol `erased` */
      def Erased: Flags

      /** Is this symbol a `def` defined in an `extension` */
      def ExtensionMethod: Flags

      /** Is this symbol a getter or a setter */
      def FieldAccessor: Flags

      /** Is this symbol `final` */
      def Final: Flags

      /** Is this symbol an inferable ("given") parameter */
      def Given: Flags

      /** Is this symbol a parameter with a default value? */
      def HasDefault: Flags

      /** Is this symbol `implicit` */
      def Implicit: Flags

      /** Is this symbol `inline` */
      def Inline: Flags

      /** Is this symbol defined in a Java class */
      def JavaDefined: Flags

      /** Is this symbol `lazy` */
      def Lazy: Flags

      /** Is this symbol local? Used in conjunction with private/private[T] to mean private[this] extends Modifier proctected[this] */
      def Local: Flags

      /** Is this symbol marked as a macro. An inline method containing toplevel splices */
      def Macro: Flags

      /** Is this symbol a module class */
      def ModuleClass: Flags

      /** Is this symbol a `var` (when used on a ValDef) */
      def Mutable: Flags

      /** Is this symbol an object or its class (used for a ValDef or a ClassDef extends Modifier respectively) */
      def Object: Flags

      /** Is this symbol `opaque` */
      def Opaque: Flags

      /** Is this symbol `open` */
      def Open: Flags

      /** Is this symbol `override` */
      def Override: Flags

      /** Is this symbol a package */
      def Package: Flags

      /** Is this symbol a parameter */
      def Param: Flags

      /** Is this symbol a parameter accessor */
      def ParamAccessor: Flags

      /** Is this symbol `private` */
      def Private: Flags

      /** Is this symbol labeled private[this] */
      def PrivateLocal: Flags

      /** Is this symbol `protected` */
      def Protected: Flags

      /** Was this symbol imported from Scala2.x */
      def Scala2x: Flags

      /** Is this symbol `sealed` */
      def Sealed: Flags

      /** Is this symbol member that is assumed to be stable and realizable */
      def StableRealizable: Flags

      /** Is this symbol marked as static. Mapped to static Java member */
      def Static: Flags

      /** Is this symbol to be tagged Java Synthetic */
      def Synthetic: Flags

      /** Is this symbol a trait */
      def Trait: Flags
    }

    /** Makes extension methods on `Flags` available without any imports */
    given FlagsMethods as FlagsMethods = FlagsMethodsImpl

    /** Implementation of extension methods on `Flags` */
    protected val FlagsMethodsImpl: FlagsMethods

    /** Extension methods of `Flags` */
    trait FlagsMethods {
      extension (self: Flags):
        /** Is the given flag set a subset of this flag sets */
        def is(that: Flags): Boolean

        /** Union of the two flag sets */
        def |(that: Flags): Flags

        /** Intersection of the two flag sets */
        def &(that: Flags): Flags

        /** Shows the tree as extractors */
        def showExtractors: String

        /** Shows the tree as fully typed source code */
        def show: String

        /** Shows the tree as fully typed source code colored with ANSI */
        def showAnsiColored: String

      end extension
    }

    ///////////////
    // POSITIONS //
    ///////////////


    /** Position in a source file */
    type Position <: AnyRef

    /** Module object of `type Position`  */
    val Position: PositionModule

    /** Methods of the module object `val Position` */
    trait PositionModule { this: Position.type =>
      /** Position of the expansion site of the macro */
      def ofMacroExpansion: Position
    }

    /** Makes extension methods on `Position` available without any imports */
    given PositionMethods as PositionMethods = PositionMethodsImpl

    /** Implementation of extension methods on `Position` */
    protected val PositionMethodsImpl: PositionMethods

    /** Extension methods of `Position` */
    trait PositionMethods {
      extension (self: Position):

        /** The start offset in the source file */
        def start: Int

        /** The end offset in the source file */
        def end: Int

        /** Does this position exist */
        def exists: Boolean

        /** Source file in which this position is located */
        def sourceFile: SourceFile

        /** The start line in the source file */
        def startLine: Int

        /** The end line in the source file */
        def endLine: Int

        /** The start column in the source file */
        def startColumn: Int

        /** The end column in the source file */
        def endColumn: Int

        /** Source code within the position */
        def sourceCode: String

      end extension
    }

    /** Scala source file */
    type SourceFile <: AnyRef

    /** Module object of `type SourceFile`  */
    val SourceFile: SourceFileModule

    /** Methods of the module object `val SourceFile` */
    trait SourceFileModule { this: SourceFile.type => }

    /** Makes extension methods on `SourceFile` available without any imports */
    given SourceFileMethods as SourceFileMethods = SourceFileMethodsImpl

    /** Implementation of extension methods on `SourceFile` */
    protected val SourceFileMethodsImpl: SourceFileMethods

    /** Extension methods of `SourceFile` */
    trait SourceFileMethods {
      extension (self: SourceFile):
        /** Path to this source file */
        def jpath: java.nio.file.Path

        /** Content of this source file */
        def content: String
      end extension
    }

    ///////////////
    //   Source  //
    ///////////////

    /** Module object of `type Source`  */
    val Source: SourceModule

    /** Methods of the module object `val Source` */
    trait SourceModule { this: Source.type =>

      /** Returns the source file being compiled. The path is relative to the current working directory. */
      def path: java.nio.file.Path

    }

    ///////////////
    // REPORTING //
    ///////////////

    /** Module containg error and waring reporiting. */
    val report: reportModule

    /** Methods of the module object `val report` */
    trait reportModule { self: report.type =>

      /** Report an error at the position of the macro expansion */
      def error(msg: String): Unit

      /** Report an error at the position of `expr` */
      def error(msg: String, expr: Expr[Any]): Unit

      /** Report an error message at the given position */
      def error(msg: String, pos: Position): Unit

      /** Report an error at a specific range of a file. The positions must be contained in the file. */
      def error(msg: String, source: SourceFile, start: Int, end: Int): Unit

      /** Report an error at the position of the macro expansion and throws a StopMacroExpansion */
      def throwError(msg: String): Nothing

      /** Report an error at the position of `expr` */
      def throwError(msg: String, expr: Expr[Any]): Nothing

      /** Report an error message at the given position and throws a StopMacroExpansion */
      def throwError(msg: String, pos: Position): Nothing

      /** Report an error at a specific range of a file and throws a StopMacroExpansion. The positions must be contained in the file. */
      def throwError(msg: String, source: SourceFile, start: Int, end: Int): Nothing

      /** Report a warning at the position of the macro expansion */
      def warning(msg: String): Unit

      /** Report a warning at the on the position of `expr` */
      def warning(msg: String, expr: Expr[Any]): Unit

      /** Report an warning message at the given position */
      def warning(msg: String, pos: Position): Unit

      /** Emits a warning at a specific range of a file. The positions must be contained in the file. */
      def warning(msg: String, source: SourceFile, start: Int, end: Int): Unit

    }


    ///////////////////
    // DOCUMENTATION //
    ///////////////////

    /** Attachment representing the documentation of a definition */
    type Documentation <: AnyRef

    /** Module object of `type Documentation`  */
    val Documentation: DocumentationModule

    /** Methods of the module object `val Documentation` */
    trait DocumentationModule { this: Documentation.type => }

    /** Makes extension methods on `Documentation` available without any imports */
    given DocumentationMethods as DocumentationMethods = DocumentationMethodsImpl

    /** Implementation of extension methods on `Documentation` */
    protected val DocumentationMethodsImpl: DocumentationMethods

    /** Extension methods of `Documentation` */
    trait DocumentationMethods {
      extension (self: Documentation):
        /** Raw documentation string */
        def raw: String

        /** Expanded documentation string, if any */
        def expanded: Option[String]

        /** List of usecases and their corresponding trees, if any */
        def usecases: List[(String, Option[DefDef])]

      end extension
    }

    ///////////////
    //   UTILS   //
    ///////////////

    /** TASTy Reflect tree accumulator.
    *
    *  Usage:
    *  ```
    *  import qctx.reflect._
    *  class MyTreeAccumulator extends TreeAccumulator[X] {
    *    def foldTree(x: X, tree: Tree)(owner: Symbol): X = ...
    *  }
    *  ```
    */
    trait TreeAccumulator[X]:

      // Ties the knot of the traversal: call `foldOver(x, tree))` to dive in the `tree` node.
      def foldTree(x: X, tree: Tree)(owner: Symbol): X

      def foldTrees(x: X, trees: Iterable[Tree])(owner: Symbol): X = trees.foldLeft(x)((acc, y) => foldTree(acc, y)(owner))

      def foldOverTree(x: X, tree: Tree)(owner: Symbol): X = {
        tree match {
          case Ident(_) =>
            x
          case Select(qualifier, _) =>
            foldTree(x, qualifier)(owner)
          case This(qual) =>
            x
          case Super(qual, _) =>
            foldTree(x, qual)(owner)
          case Apply(fun, args) =>
            foldTrees(foldTree(x, fun)(owner), args)(owner)
          case TypeApply(fun, args) =>
            foldTrees(foldTree(x, fun)(owner), args)(owner)
          case Literal(const) =>
            x
          case New(tpt) =>
            foldTree(x, tpt)(owner)
          case Typed(expr, tpt) =>
            foldTree(foldTree(x, expr)(owner), tpt)(owner)
          case NamedArg(_, arg) =>
            foldTree(x, arg)(owner)
          case Assign(lhs, rhs) =>
            foldTree(foldTree(x, lhs)(owner), rhs)(owner)
          case Block(stats, expr) =>
            foldTree(foldTrees(x, stats)(owner), expr)(owner)
          case If(cond, thenp, elsep) =>
            foldTree(foldTree(foldTree(x, cond)(owner), thenp)(owner), elsep)(owner)
          case While(cond, body) =>
            foldTree(foldTree(x, cond)(owner), body)(owner)
          case Closure(meth, tpt) =>
            foldTree(x, meth)(owner)
          case Match(selector, cases) =>
            foldTrees(foldTree(x, selector)(owner), cases)(owner)
          case Return(expr, _) =>
            foldTree(x, expr)(owner)
          case Try(block, handler, finalizer) =>
            foldTrees(foldTrees(foldTree(x, block)(owner), handler)(owner), finalizer)(owner)
          case Repeated(elems, elemtpt) =>
            foldTrees(foldTree(x, elemtpt)(owner), elems)(owner)
          case Inlined(call, bindings, expansion) =>
            foldTree(foldTrees(x, bindings)(owner), expansion)(owner)
          case vdef @ ValDef(_, tpt, rhs) =>
            val owner = vdef.symbol
            foldTrees(foldTree(x, tpt)(owner), rhs)(owner)
          case ddef @ DefDef(_, tparams, vparamss, tpt, rhs) =>
            val owner = ddef.symbol
            foldTrees(foldTree(vparamss.foldLeft(foldTrees(x, tparams)(owner))((acc, y) => foldTrees(acc, y)(owner)), tpt)(owner), rhs)(owner)
          case tdef @ TypeDef(_, rhs) =>
            val owner = tdef.symbol
            foldTree(x, rhs)(owner)
          case cdef @ ClassDef(_, constr, parents, derived, self, body) =>
            val owner = cdef.symbol
            foldTrees(foldTrees(foldTrees(foldTrees(foldTree(x, constr)(owner), parents)(owner), derived)(owner), self)(owner), body)(owner)
          case Import(expr, _) =>
            foldTree(x, expr)(owner)
          case clause @ PackageClause(pid, stats) =>
            foldTrees(foldTree(x, pid)(owner), stats)(clause.symbol)
          case Inferred() => x
          case TypeIdent(_) => x
          case TypeSelect(qualifier, _) => foldTree(x, qualifier)(owner)
          case TypeProjection(qualifier, _) => foldTree(x, qualifier)(owner)
          case Singleton(ref) => foldTree(x, ref)(owner)
          case Refined(tpt, refinements) => foldTrees(foldTree(x, tpt)(owner), refinements)(owner)
          case Applied(tpt, args) => foldTrees(foldTree(x, tpt)(owner), args)(owner)
          case ByName(result) => foldTree(x, result)(owner)
          case Annotated(arg, annot) => foldTree(foldTree(x, arg)(owner), annot)(owner)
          case LambdaTypeTree(typedefs, arg) => foldTree(foldTrees(x, typedefs)(owner), arg)(owner)
          case TypeBind(_, tbt) => foldTree(x, tbt)(owner)
          case TypeBlock(typedefs, tpt) => foldTree(foldTrees(x, typedefs)(owner), tpt)(owner)
          case MatchTypeTree(boundopt, selector, cases) =>
            foldTrees(foldTree(boundopt.fold(x)(y => foldTree(x, y)(owner)), selector)(owner), cases)(owner)
          case WildcardTypeTree() => x
          case TypeBoundsTree(lo, hi) => foldTree(foldTree(x, lo)(owner), hi)(owner)
          case CaseDef(pat, guard, body) => foldTree(foldTrees(foldTree(x, pat)(owner), guard)(owner), body)(owner)
          case TypeCaseDef(pat, body) => foldTree(foldTree(x, pat)(owner), body)(owner)
          case Bind(_, body) => foldTree(x, body)(owner)
          case Unapply(fun, implicits, patterns) => foldTrees(foldTrees(foldTree(x, fun)(owner), implicits)(owner), patterns)(owner)
          case Alternatives(patterns) => foldTrees(x, patterns)(owner)
        }
      }
    end TreeAccumulator


    /** TASTy Reflect tree traverser.
    *
    *  Usage:
    *  ```
    *  import qctx.relfect._
    *  class MyTraverser extends TreeTraverser {
    *    override def traverseTree(tree: Tree)(owner: Symbol): Unit = ...
    *  }
    *  ```
    */
    trait TreeTraverser extends TreeAccumulator[Unit]:

      def traverseTree(tree: Tree)(owner: Symbol): Unit = traverseTreeChildren(tree)(owner)

      def foldTree(x: Unit, tree: Tree)(owner: Symbol): Unit = traverseTree(tree)(owner)

      protected def traverseTreeChildren(tree: Tree)(owner: Symbol): Unit = foldOverTree((), tree)(owner)

    end TreeTraverser

    /** TASTy Reflect tree map.
    *
    *  Usage:
    *  ```
    *  import quotes.reflect._
    *  class MyTreeMap extends TreeMap {
    *    override def transformTree(tree: Tree)(owner: Symbol): Tree = ...
    *  }
    *  ```
    */
    trait TreeMap:

      def transformTree(tree: Tree)(owner: Symbol): Tree = {
        tree match {
          case tree: PackageClause =>
            PackageClause.copy(tree)(transformTerm(tree.pid).asInstanceOf[Ref], transformTrees(tree.stats)(tree.symbol))
          case tree: Import =>
            Import.copy(tree)(transformTerm(tree.expr)(owner), tree.selectors)
          case tree: Statement =>
            transformStatement(tree)(owner)
          case tree: TypeTree => transformTypeTree(tree)(owner)
          case tree: TypeBoundsTree =>
            TypeBoundsTree.copy(tree)(transformTypeTree(tree.low)(owner), transformTypeTree(tree.hi)(owner))
          case tree: WildcardTypeTree => tree
          case tree: CaseDef =>
            transformCaseDef(tree)(owner)
          case tree: TypeCaseDef =>
            transformTypeCaseDef(tree)(owner)
          case pattern: Bind =>
            Bind.copy(pattern)(pattern.name, pattern.pattern)
          case pattern: Unapply =>
            Unapply.copy(pattern)(transformTerm(pattern.fun)(owner), transformSubTrees(pattern.implicits)(owner), transformTrees(pattern.patterns)(owner))
          case pattern: Alternatives =>
            Alternatives.copy(pattern)(transformTrees(pattern.patterns)(owner))
        }
      }

      def transformStatement(tree: Statement)(owner: Symbol): Statement = {
        tree match {
          case tree: Term =>
            transformTerm(tree)(owner)
          case tree: ValDef =>
            val owner = tree.symbol
            val tpt1 = transformTypeTree(tree.tpt)(owner)
            val rhs1 = tree.rhs.map(x => transformTerm(x)(owner))
            ValDef.copy(tree)(tree.name, tpt1, rhs1)
          case tree: DefDef =>
            val owner = tree.symbol
            DefDef.copy(tree)(tree.name, transformSubTrees(tree.typeParams)(owner), tree.paramss mapConserve (x => transformSubTrees(x)(owner)), transformTypeTree(tree.returnTpt)(owner), tree.rhs.map(x => transformTerm(x)(owner)))
          case tree: TypeDef =>
            val owner = tree.symbol
            TypeDef.copy(tree)(tree.name, transformTree(tree.rhs)(owner))
          case tree: ClassDef =>
            ClassDef.copy(tree)(tree.name, tree.constructor, tree.parents, tree.derived, tree.self, tree.body)
          case tree: Import =>
            Import.copy(tree)(transformTerm(tree.expr)(owner), tree.selectors)
        }
      }

      def transformTerm(tree: Term)(owner: Symbol): Term = {
        tree match {
          case Ident(name) =>
            tree
          case Select(qualifier, name) =>
            Select.copy(tree)(transformTerm(qualifier)(owner), name)
          case This(qual) =>
            tree
          case Super(qual, mix) =>
            Super.copy(tree)(transformTerm(qual)(owner), mix)
          case Apply(fun, args) =>
            Apply.copy(tree)(transformTerm(fun)(owner), transformTerms(args)(owner))
          case TypeApply(fun, args) =>
            TypeApply.copy(tree)(transformTerm(fun)(owner), transformTypeTrees(args)(owner))
          case Literal(const) =>
            tree
          case New(tpt) =>
            New.copy(tree)(transformTypeTree(tpt)(owner))
          case Typed(expr, tpt) =>
            Typed.copy(tree)(transformTerm(expr)(owner), transformTypeTree(tpt)(owner))
          case tree: NamedArg =>
            NamedArg.copy(tree)(tree.name, transformTerm(tree.value)(owner))
          case Assign(lhs, rhs) =>
            Assign.copy(tree)(transformTerm(lhs)(owner), transformTerm(rhs)(owner))
          case Block(stats, expr) =>
            Block.copy(tree)(transformStats(stats)(owner), transformTerm(expr)(owner))
          case If(cond, thenp, elsep) =>
            If.copy(tree)(transformTerm(cond)(owner), transformTerm(thenp)(owner), transformTerm(elsep)(owner))
          case Closure(meth, tpt) =>
            Closure.copy(tree)(transformTerm(meth)(owner), tpt)
          case Match(selector, cases) =>
            Match.copy(tree)(transformTerm(selector)(owner), transformCaseDefs(cases)(owner))
          case Return(expr, from) =>
            Return.copy(tree)(transformTerm(expr)(owner), from)
          case While(cond, body) =>
            While.copy(tree)(transformTerm(cond)(owner), transformTerm(body)(owner))
          case Try(block, cases, finalizer) =>
            Try.copy(tree)(transformTerm(block)(owner), transformCaseDefs(cases)(owner), finalizer.map(x => transformTerm(x)(owner)))
          case Repeated(elems, elemtpt) =>
            Repeated.copy(tree)(transformTerms(elems)(owner), transformTypeTree(elemtpt)(owner))
          case Inlined(call, bindings, expansion) =>
            Inlined.copy(tree)(call, transformSubTrees(bindings)(owner), transformTerm(expansion)(owner))
        }
      }

      def transformTypeTree(tree: TypeTree)(owner: Symbol): TypeTree = tree match {
        case Inferred() => tree
        case tree: TypeIdent => tree
        case tree: TypeSelect =>
          TypeSelect.copy(tree)(tree.qualifier, tree.name)
        case tree: TypeProjection =>
          TypeProjection.copy(tree)(tree.qualifier, tree.name)
        case tree: Annotated =>
          Annotated.copy(tree)(tree.arg, tree.annotation)
        case tree: Singleton =>
          Singleton.copy(tree)(transformTerm(tree.ref)(owner))
        case tree: Refined =>
          Refined.copy(tree)(transformTypeTree(tree.tpt)(owner), transformTrees(tree.refinements)(owner).asInstanceOf[List[Definition]])
        case tree: Applied =>
          Applied.copy(tree)(transformTypeTree(tree.tpt)(owner), transformTrees(tree.args)(owner))
        case tree: MatchTypeTree =>
          MatchTypeTree.copy(tree)(tree.bound.map(b => transformTypeTree(b)(owner)), transformTypeTree(tree.selector)(owner), transformTypeCaseDefs(tree.cases)(owner))
        case tree: ByName =>
          ByName.copy(tree)(transformTypeTree(tree.result)(owner))
        case tree: LambdaTypeTree =>
          LambdaTypeTree.copy(tree)(transformSubTrees(tree.tparams)(owner), transformTree(tree.body)(owner))
        case tree: TypeBind =>
          TypeBind.copy(tree)(tree.name, tree.body)
        case tree: TypeBlock =>
          TypeBlock.copy(tree)(tree.aliases, tree.tpt)
      }

      def transformCaseDef(tree: CaseDef)(owner: Symbol): CaseDef = {
        CaseDef.copy(tree)(transformTree(tree.pattern)(owner), tree.guard.map(x => transformTerm(x)(owner)), transformTerm(tree.rhs)(owner))
      }

      def transformTypeCaseDef(tree: TypeCaseDef)(owner: Symbol): TypeCaseDef = {
        TypeCaseDef.copy(tree)(transformTypeTree(tree.pattern)(owner), transformTypeTree(tree.rhs)(owner))
      }

      def transformStats(trees: List[Statement])(owner: Symbol): List[Statement] =
        trees mapConserve (x => transformStatement(x)(owner))

      def transformTrees(trees: List[Tree])(owner: Symbol): List[Tree] =
        trees mapConserve (x => transformTree(x)(owner))

      def transformTerms(trees: List[Term])(owner: Symbol): List[Term] =
        trees mapConserve (x => transformTerm(x)(owner))

      def transformTypeTrees(trees: List[TypeTree])(owner: Symbol): List[TypeTree] =
        trees mapConserve (x => transformTypeTree(x)(owner))

      def transformCaseDefs(trees: List[CaseDef])(owner: Symbol): List[CaseDef] =
        trees mapConserve (x => transformCaseDef(x)(owner))

      def transformTypeCaseDefs(trees: List[TypeCaseDef])(owner: Symbol): List[TypeCaseDef] =
        trees mapConserve (x => transformTypeCaseDef(x)(owner))

      def transformSubTrees[Tr <: Tree](trees: List[Tr])(owner: Symbol): List[Tr] =
        transformTrees(trees)(owner).asInstanceOf[List[Tr]]

    end TreeMap

  }

  /** Type of a `Quotes` provided by a splice within a quote that took this context. */
  type Nested = Quotes

}
