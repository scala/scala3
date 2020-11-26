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

    val Tree: TreeModule

    trait TreeModule { this: Tree.type =>
      /** Returns the Term representation this expression */
      def of(expr: Expr[Any]): Tree
    }

    given TreeMethods as TreeMethods = TreeMethodsImpl
    protected val TreeMethodsImpl: TreeMethods

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

    given TypeTest[Tree, PackageClause] = PackageClauseTypeTest
    protected val PackageClauseTypeTest: TypeTest[Tree, PackageClause]

    val PackageClause: PackageClauseModule

    trait PackageClauseModule { this: PackageClause.type =>
      def apply(pid: Ref, stats: List[Tree]): PackageClause
      def copy(original: Tree)(pid: Ref, stats: List[Tree]): PackageClause
      def unapply(tree: PackageClause): Some[(Ref, List[Tree])]
    }

    given PackageClauseMethods as PackageClauseMethods = PackageClauseMethodsImpl
    protected val PackageClauseMethodsImpl: PackageClauseMethods

    trait PackageClauseMethods:
      extension (self: PackageClause):
        def pid: Ref
        def stats: List[Tree]
      end extension
    end PackageClauseMethods

    /** Tree representing an import in the source code */
    type Import <: Statement

    given TypeTest[Tree, Import] = ImportTypeTest
    protected val ImportTypeTest: TypeTest[Tree, Import]

    val Import: ImportModule

    trait ImportModule { this: Import.type =>
      def apply(expr: Term, selectors: List[ImportSelector]): Import
      def copy(original: Tree)(expr: Term, selectors: List[ImportSelector]): Import
      def unapply(tree: Import): Option[(Term, List[ImportSelector])]
    }

    given ImportMethods as ImportMethods = ImportMethodsImpl
    protected val ImportMethodsImpl: ImportMethods

    trait ImportMethods:
      extension (self: Import):
        def expr: Term
        def selectors: List[ImportSelector]
      end extension
    end ImportMethods

    /** Tree representing a statement in the source code */
    type Statement <: Tree

    given TypeTest[Tree, Statement] = StatementTypeTest
    protected val StatementTypeTest: TypeTest[Tree, Statement]

    // ----- Definitions ----------------------------------------------

    /** Tree representing a definition in the source code. It can be `ClassDef`, `TypeDef`, `DefDef` or `ValDef` */
    type Definition <: Statement

    given TypeTest[Tree, Definition] = DefinitionTypeTest
    protected val DefinitionTypeTest: TypeTest[Tree, Definition]

    val Definition: DefinitionModule

    trait DefinitionModule { this: Definition.type => }

    given DefinitionMethods as DefinitionMethods = DefinitionMethodsImpl
    protected val DefinitionMethodsImpl: DefinitionMethods

    trait DefinitionMethods:
      extension (self: Definition):
        def name: String
      end extension
    end DefinitionMethods

    // ClassDef

    /** Tree representing a class definition. This includes anonymous class definitions and the class of a module object */
    type ClassDef <: Definition

    given TypeTest[Tree, ClassDef] = ClassDefTypeTest
    protected val ClassDefTypeTest: TypeTest[Tree, ClassDef]

    val ClassDef: ClassDefModule

    trait ClassDefModule { this: ClassDef.type =>
      // TODO def apply(name: String, constr: DefDef, parents: List[TermOrTypeTree], selfOpt: Option[ValDef], body: List[Statement]): ClassDef
      def copy(original: Tree)(name: String, constr: DefDef, parents: List[Tree /* Term | TypeTree */], derived: List[TypeTree], selfOpt: Option[ValDef], body: List[Statement]): ClassDef
      def unapply(cdef: ClassDef): Option[(String, DefDef, List[Tree /* Term | TypeTree */], List[TypeTree], Option[ValDef], List[Statement])]
    }

    given ClassDefMethods as ClassDefMethods = ClassDefMethodsImpl
    protected val ClassDefMethodsImpl: ClassDefMethods

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

    given TypeTest[Tree, DefDef] = DefDefTypeTest
    protected val DefDefTypeTest: TypeTest[Tree, DefDef]

    val DefDef: DefDefModule

    trait DefDefModule { this: DefDef.type =>
      def apply(symbol: Symbol, rhsFn: List[TypeRepr] => List[List[Term]] => Option[Term]): DefDef
      def copy(original: Tree)(name: String, typeParams: List[TypeDef], paramss: List[List[ValDef]], tpt: TypeTree, rhs: Option[Term]): DefDef
      def unapply(ddef: DefDef): Option[(String, List[TypeDef], List[List[ValDef]], TypeTree, Option[Term])]
    }

    given DefDefMethods as DefDefMethods = DefDefMethodsImpl
    protected val DefDefMethodsImpl: DefDefMethods

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

    given TypeTest[Tree, ValDef] = ValDefTypeTest
    protected val ValDefTypeTest: TypeTest[Tree, ValDef]

    val ValDef: ValDefModule

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

    given ValDefMethods as ValDefMethods = ValDefMethodsImpl
    protected val ValDefMethodsImpl: ValDefMethods

    trait ValDefMethods:
      extension (self: ValDef):
        def tpt: TypeTree
        def rhs: Option[Term]
      end extension
    end ValDefMethods

    // TypeDef

    /** Tree representing a type (parameter or member) definition in the source code */
    type TypeDef <: Definition

    given TypeTest[Tree, TypeDef] = TypeDefTypeTest
    protected val TypeDefTypeTest: TypeTest[Tree, TypeDef]

    val TypeDef: TypeDefModule

    trait TypeDefModule { this: TypeDef.type =>
      def apply(symbol: Symbol): TypeDef
      def copy(original: Tree)(name: String, rhs: Tree /*TypeTree | TypeBoundsTree*/): TypeDef
      def unapply(tdef: TypeDef): Option[(String, Tree /*TypeTree | TypeBoundsTree*/ /* TypeTree | TypeBoundsTree */)]
    }

    given TypeDefMethods as TypeDefMethods = TypeDefMethodsImpl
    protected val TypeDefMethodsImpl: TypeDefMethods

    trait TypeDefMethods:
      extension (self: TypeDef):
        def rhs: Tree /*TypeTree | TypeBoundsTree*/
      end extension
    end TypeDefMethods


    // ----- Terms ----------------------------------------------------

    /** Tree representing an expression in the source code */
    type Term <: Statement

    given TypeTest[Tree, Term] = TermTypeTest
    protected val TermTypeTest: TypeTest[Tree, Term]

    val Term: TermModule

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

    given TermMethods as TermMethods = TermMethodsImpl
    protected val TermMethodsImpl: TermMethods

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

    given TypeTest[Tree, Ref] = RefTypeTest
    protected val RefTypeTest: TypeTest[Tree, Ref]

    val Ref: RefModule

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

    given TypeTest[Tree, Ident] = IdentTypeTest
    protected val IdentTypeTest: TypeTest[Tree, Ident]

    /** Scala term identifier */
    val Ident: IdentModule

    trait IdentModule { this: Ident.type =>
      def apply(tmref: TermRef): Term

      def copy(original: Tree)(name: String): Ident

      /** Matches a term identifier and returns its name */
      def unapply(tree: Ident): Option[String]
    }

    given IdentMethods as IdentMethods = IdentMethodsImpl
    protected val IdentMethodsImpl: IdentMethods

    trait IdentMethods:
      extension (self: Ident):
        def name: String
      end extension
    end IdentMethods

    /** Tree representing a selection of definition with a given name on a given prefix */
    type Select <: Ref

    given TypeTest[Tree, Select] = SelectTypeTest
    protected val SelectTypeTest: TypeTest[Tree, Select]

    /** Scala term selection */
    val Select: SelectModule

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

    given SelectMethods as SelectMethods = SelectMethodsImpl
    protected val SelectMethodsImpl: SelectMethods

    trait SelectMethods:
      extension (self: Select):
        def qualifier: Term
        def name: String
        def signature: Option[Signature]
      end extension
    end SelectMethods

    given TypeTest[Tree, Literal] = LiteralTypeTest
    protected val LiteralTypeTest: TypeTest[Tree, Literal]

    /** Tree representing a literal value in the source code */
    type Literal <: Term

    /** Scala literal constant */
    val Literal: LiteralModule

    trait LiteralModule { this: Literal.type =>

      /** Create a literal constant */
      def apply(constant: Constant): Literal

      def copy(original: Tree)(constant: Constant): Literal

      /** Matches a literal constant */
      def unapply(x: Literal): Option[Constant]
    }

    given LiteralMethods as LiteralMethods = LiteralMethodsImpl
    protected val LiteralMethodsImpl: LiteralMethods

    trait LiteralMethods:
      extension (self: Literal):
        def constant: Constant
      end extension
    end LiteralMethods

    /** Tree representing `this` in the source code */
    type This <: Term

    given TypeTest[Tree, This] = ThisTypeTest
    protected val ThisTypeTest: TypeTest[Tree, This]

    /** Scala `this` or `this[id]` */
    val This: ThisModule

    trait ThisModule { this: This.type =>

      /** Create a `this[<id: String]>` */
      def apply(cls: Symbol): This

      def copy(original: Tree)(qual: Option[String]): This

      /** Matches `this[<id: Option[String]>` */
      def unapply(x: This): Option[Option[String]]
    }

    given ThisMethods as ThisMethods = ThisMethodsImpl
    protected val ThisMethodsImpl: ThisMethods

    trait ThisMethods:
      extension (self: This):
        def id: Option[String]
      end extension
    end ThisMethods

    /** Tree representing `new` in the source code */
    type New <: Term

    given TypeTest[Tree, New] = NewTypeTest
    protected val NewTypeTest: TypeTest[Tree, New]

    /** Scala `new` */
    val New: NewModule

    trait NewModule { this: New.type =>

      /** Create a `new <tpt: TypeTree>` */
      def apply(tpt: TypeTree): New

      def copy(original: Tree)(tpt: TypeTree): New

      /** Matches a `new <tpt: TypeTree>` */
      def unapply(x: New): Option[TypeTree]
    }

    given NewMethods as NewMethods = NewMethodsImpl
    protected val NewMethodsImpl: NewMethods

    trait NewMethods:
      extension (self: New):
        def tpt: TypeTree
      end extension
    end NewMethods

    /** Tree representing an argument passed with an explicit name. Such as `arg1 = x` in `foo(arg1 = x)` */
    type NamedArg <: Term

    given TypeTest[Tree, NamedArg] = NamedArgTypeTest
    protected val NamedArgTypeTest: TypeTest[Tree, NamedArg]

    /** Scala named argument `x = y` in argument position */
    val NamedArg: NamedArgModule

    trait NamedArgModule { this: NamedArg.type =>

      /** Create a named argument `<name: String> = <value: Term>` */
      def apply(name: String, arg: Term): NamedArg

      def copy(original: Tree)(name: String, arg: Term): NamedArg

      /** Matches a named argument `<name: String> = <value: Term>` */
      def unapply(x: NamedArg): Option[(String, Term)]
    }

    given NamedArgMethods as NamedArgMethods = NamedArgMethodsImpl
    protected val NamedArgMethodsImpl: NamedArgMethods

    trait NamedArgMethods:
      extension (self: NamedArg):
        def name: String
        def value: Term
      end extension
    end NamedArgMethods

    /** Tree an application of arguments. It represents a single list of arguments, multiple argument lists will have nested `Apply`s  */
    type Apply <: Term

    given TypeTest[Tree, Apply] = ApplyTypeTest
    protected val ApplyTypeTest: TypeTest[Tree, Apply]

    /** Scala parameter application */
    val Apply: ApplyModule

    trait ApplyModule { this: Apply.type =>

      /** Create a function application `<fun: Term>(<args: List[Term]>)` */
      def apply(fun: Term, args: List[Term]): Apply

      def copy(original: Tree)(fun: Term, args: List[Term]): Apply

      /** Matches a function application `<fun: Term>(<args: List[Term]>)` */
      def unapply(x: Apply): Option[(Term, List[Term])]
    }

    given ApplyMethods as ApplyMethods = ApplyMethodsImpl
    protected val ApplyMethodsImpl: ApplyMethods

    trait ApplyMethods:
      extension (self: Apply):
        def fun: Term
        def args: List[Term]
      end extension
    end ApplyMethods

    /** Tree an application of type arguments */
    type TypeApply <: Term

    given TypeTest[Tree, TypeApply] = TypeApplyTypeTest
    protected val TypeApplyTypeTest: TypeTest[Tree, TypeApply]

    /** Scala type parameter application */
    val TypeApply: TypeApplyModule

    trait TypeApplyModule { this: TypeApply.type =>

      /** Create a function type application `<fun: Term>[<args: List[TypeTree]>]` */
      def apply(fun: Term, args: List[TypeTree]): TypeApply

      def copy(original: Tree)(fun: Term, args: List[TypeTree]): TypeApply

      /** Matches a function type application `<fun: Term>[<args: List[TypeTree]>]` */
      def unapply(x: TypeApply): Option[(Term, List[TypeTree])]
    }

    given TypeApplyMethods as TypeApplyMethods = TypeApplyMethodsImpl
    protected val TypeApplyMethodsImpl: TypeApplyMethods

    trait TypeApplyMethods:
      extension (self: TypeApply):
        def fun: Term
        def args: List[TypeTree]
      end extension
    end TypeApplyMethods

    given TypeTest[Tree, Super] = SuperTypeTest
    protected val SuperTypeTest: TypeTest[Tree, Super]

    /** Tree representing `super` in the source code */
    type Super <: Term

    /** Scala `x.super` or `x.super[id]` */
    val Super: SuperModule

    trait SuperModule { this: Super.type =>

      /** Creates a `<qualifier: Term>.super[<id: Option[Id]>` */
      def apply(qual: Term, mix: Option[String]): Super

      def copy(original: Tree)(qual: Term, mix: Option[String]): Super

      /** Matches a `<qualifier: Term>.super[<id: Option[Id]>` */
      def unapply(x: Super): Option[(Term, Option[String])]
    }

    given SuperMethods as SuperMethods = SuperMethodsImpl
    protected val SuperMethodsImpl: SuperMethods

    trait SuperMethods:
      extension (self: Super):
        def qualifier: Term
        def id: Option[String]
        def idPos: Position
      end extension
    end SuperMethods

    given TypeTest[Tree, Typed] = TypedTypeTest
    protected val TypedTypeTest: TypeTest[Tree, Typed]

    /** Tree representing a type ascription `x: T` in the source code */
    type Typed <: Term

    /** Scala ascription `x: T` */
    val Typed: TypedModule

    trait TypedModule { this: Typed.type =>

      /** Create a type ascription `<x: Term>: <tpt: TypeTree>` */
      def apply(expr: Term, tpt: TypeTree): Typed

      def copy(original: Tree)(expr: Term, tpt: TypeTree): Typed

      /** Matches `<expr: Term>: <tpt: TypeTree>` */
      def unapply(x: Typed): Option[(Term, TypeTree)]
    }

    given TypedMethods as TypedMethods = TypedMethodsImpl
    protected val TypedMethodsImpl: TypedMethods

    trait TypedMethods:
      extension (self: Typed):
        def expr: Term
        def tpt: TypeTree
      end extension
    end TypedMethods

    /** Tree representing an assignment `x = y` in the source code */
    type Assign <: Term

    given TypeTest[Tree, Assign] = AssignTypeTest
    protected val AssignTypeTest: TypeTest[Tree, Assign]

    /** Scala assign `x = y` */
    val Assign: AssignModule

    trait AssignModule { this: Assign.type =>

      /** Create an assignment `<lhs: Term> = <rhs: Term>` */
      def apply(lhs: Term, rhs: Term): Assign

      def copy(original: Tree)(lhs: Term, rhs: Term): Assign

      /** Matches an assignment `<lhs: Term> = <rhs: Term>` */
      def unapply(x: Assign): Option[(Term, Term)]
    }

    given AssignMethods as AssignMethods = AssignMethodsImpl
    protected val AssignMethodsImpl: AssignMethods

    trait AssignMethods:
      extension (self: Assign):
        def lhs: Term
        def rhs: Term
      end extension
    end AssignMethods

    /** Tree representing a block `{ ... }` in the source code */
    type Block <: Term

    given TypeTest[Tree, Block] = BlockTypeTest
    protected val BlockTypeTest: TypeTest[Tree, Block]

    /** Scala code block `{ stat0; ...; statN; expr }` term */
    val Block: BlockModule

    trait BlockModule { this: Block.type =>

      /** Creates a block `{ <statements: List[Statement]>; <expr: Term> }` */
      def apply(stats: List[Statement], expr: Term): Block

      def copy(original: Tree)(stats: List[Statement], expr: Term): Block

      /** Matches a block `{ <statements: List[Statement]>; <expr: Term> }` */
      def unapply(x: Block): Option[(List[Statement], Term)]
    }

    given BlockMethods as BlockMethods = BlockMethodsImpl
    protected val BlockMethodsImpl: BlockMethods

    trait BlockMethods:
      extension (self: Block):
        def statements: List[Statement]
        def expr: Term
      end extension
    end BlockMethods

    given TypeTest[Tree, Closure] = ClosureTypeTest
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

    val Closure: ClosureModule

    trait ClosureModule { this: Closure.type =>

      def apply(meth: Term, tpe: Option[TypeRepr]): Closure

      def copy(original: Tree)(meth: Tree, tpe: Option[TypeRepr]): Closure

      def unapply(x: Closure): Option[(Term, Option[TypeRepr])]
    }

    given ClosureMethods as ClosureMethods = ClosureMethodsImpl
    protected val ClosureMethodsImpl: ClosureMethods

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

    given TypeTest[Tree, If] = IfTypeTest
    protected val IfTypeTest: TypeTest[Tree, If]

    /** Tree representing an if/then/else `if (...) ... else ...` in the source code */
    type If <: Term

    /** Scala `if`/`else` term */
    val If: IfModule

    trait IfModule { this: If.type =>

      /** Create an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>` */
      def apply(cond: Term, thenp: Term, elsep: Term): If

      def copy(original: Tree)(cond: Term, thenp: Term, elsep: Term): If

      /** Matches an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>` */
      def unapply(tree: If): Option[(Term, Term, Term)]
    }

    given IfMethods as IfMethods = IfMethodsImpl
    protected val IfMethodsImpl: IfMethods

    trait IfMethods:
      extension (self: If):
        def cond: Term
        def thenp: Term
        def elsep: Term
      end extension
    end IfMethods

    /** Tree representing a pattern match `x match  { ... }` in the source code */
    type Match <: Term

    given TypeTest[Tree, Match] = MatchTypeTest
    protected val MatchTypeTest: TypeTest[Tree, Match]

    /** Scala `match` term */
    val Match: MatchModule

    trait MatchModule { this: Match.type =>

      /** Creates a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }` */
      def apply(selector: Term, cases: List[CaseDef]): Match

      def copy(original: Tree)(selector: Term, cases: List[CaseDef]): Match

      /** Matches a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }` */
      def unapply(x: Match): Option[(Term, List[CaseDef])]
    }

    given MatchMethods as MatchMethods = MatchMethodsImpl
    protected val MatchMethodsImpl: MatchMethods

    trait MatchMethods:
      extension (self: Match):
        def scrutinee: Term
        def cases: List[CaseDef]
      end extension
    end MatchMethods

    /** Tree representing a summoning match `summonFrom { ... }` in the source code */
    type SummonFrom <: Term

    given TypeTest[Tree, SummonFrom] = SummonFromTypeTest
    protected val SummonFromTypeTest: TypeTest[Tree, SummonFrom]

    /** Scala implicit `match` term */
    val SummonFrom: SummonFromModule

    trait SummonFromModule { this: SummonFrom.type =>

      /** Creates a pattern match `given match { <cases: List[CaseDef]> }` */
      def apply(cases: List[CaseDef]): SummonFrom

      def copy(original: Tree)(cases: List[CaseDef]): SummonFrom

      /** Matches a pattern match `given match { <cases: List[CaseDef]> }` */
      def unapply(x: SummonFrom): Option[List[CaseDef]]
    }

    given SummonFromMethods as SummonFromMethods = SummonFromMethodsImpl
    protected val SummonFromMethodsImpl: SummonFromMethods

    trait SummonFromMethods:
      extension (self: SummonFrom):
        def cases: List[CaseDef]
      end extension
    end SummonFromMethods

    /** Tree representing a try catch `try x catch { ... } finally { ... }` in the source code */
    type Try <: Term

    given TypeTest[Tree, Try] = TryTypeTest
    protected val TryTypeTest: TypeTest[Tree, Try]

    /** Scala `try`/`catch`/`finally` term */
    val Try: TryModule

    trait TryModule { this: Try.type =>

      /** Create a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>` */
      def apply(expr: Term, cases: List[CaseDef], finalizer: Option[Term]): Try

      def copy(original: Tree)(expr: Term, cases: List[CaseDef], finalizer: Option[Term]): Try

      /** Matches a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>` */
      def unapply(x: Try): Option[(Term, List[CaseDef], Option[Term])]
    }

    given TryMethods as TryMethods = TryMethodsImpl
    protected val TryMethodsImpl: TryMethods

    trait TryMethods:
      extension (self: Try):
        def body: Term
        def cases: List[CaseDef]
        def finalizer: Option[Term]
      end extension
    end TryMethods

    given TypeTest[Tree, Return] = ReturnTypeTest
    protected val ReturnTypeTest: TypeTest[Tree, Return]

    /** Tree representing a `return` in the source code */
    type Return <: Term

    /** Scala local `return` */
    val Return: ReturnModule

    trait ReturnModule { this: Return.type =>

      /** Creates `return <expr: Term>` */
      def apply(expr: Term, from: Symbol): Return

      def copy(original: Tree)(expr: Term, from: Symbol): Return

      /** Matches `return <expr: Term>` and extracts the expression and symbol of the method */
      def unapply(x: Return): Option[(Term, Symbol)]
    }

    given ReturnMethods as ReturnMethods = ReturnMethodsImpl
    protected val ReturnMethodsImpl: ReturnMethods

    trait ReturnMethods:
      extension (self: Return):
        def expr: Term
        def from: Symbol
      end extension
    end ReturnMethods

    /** Tree representing a variable argument list in the source code */
    type Repeated <: Term

    given TypeTest[Tree, Repeated] = RepeatedTypeTest
    protected val RepeatedTypeTest: TypeTest[Tree, Repeated]

    val Repeated: RepeatedModule

    trait RepeatedModule { this: Repeated.type =>
      def apply(elems: List[Term], tpt: TypeTree): Repeated
      def copy(original: Tree)(elems: List[Term], tpt: TypeTree): Repeated
      def unapply(x: Repeated): Option[(List[Term], TypeTree)]
    }

    given RepeatedMethods as RepeatedMethods = RepeatedMethodsImpl
    protected val RepeatedMethodsImpl: RepeatedMethods

    trait RepeatedMethods:
      extension (self: Repeated):
        def elems: List[Term]
        def elemtpt: TypeTree
      end extension
    end RepeatedMethods

    /** Tree representing the scope of an inlined tree */
    type Inlined <: Term

    given TypeTest[Tree, Inlined] = InlinedTypeTest
    protected val InlinedTypeTest: TypeTest[Tree, Inlined]

    val Inlined: InlinedModule

    trait InlinedModule { this: Inlined.type =>
      def apply(call: Option[Tree /* Term | TypeTree */], bindings: List[Definition], expansion: Term): Inlined
      def copy(original: Tree)(call: Option[Tree /* Term | TypeTree */], bindings: List[Definition], expansion: Term): Inlined
      def unapply(x: Inlined): Option[(Option[Tree /* Term | TypeTree */], List[Definition], Term)]
    }

    given InlinedMethods as InlinedMethods = InlinedMethodsImpl
    protected val InlinedMethodsImpl: InlinedMethods

    trait InlinedMethods:
      extension (self: Inlined):
        def call: Option[Tree /* Term | TypeTree */]
        def bindings: List[Definition]
        def body: Term
      end extension
    end InlinedMethods

    /** Tree representing a selection of definition with a given name on a given prefix and number of nested scopes of inlined trees */
    type SelectOuter <: Term

    given TypeTest[Tree, SelectOuter] = SelectOuterTypeTest
    protected val SelectOuterTypeTest: TypeTest[Tree, SelectOuter]

    val SelectOuter: SelectOuterModule

    trait SelectOuterModule { this: SelectOuter.type =>
      def apply(qualifier: Term, name: String, levels: Int): SelectOuter
      def copy(original: Tree)(qualifier: Term, name: String, levels: Int): SelectOuter
      def unapply(x: SelectOuter): Option[(Term, String, Int)]
    }

    given SelectOuterMethods as SelectOuterMethods = SelectOuterMethodsImpl
    protected val SelectOuterMethodsImpl: SelectOuterMethods

    trait SelectOuterMethods:
      extension (self: SelectOuter):
        def qualifier: Term
        def name: String
        def level: Int
      end extension
    end SelectOuterMethods

    /** Tree representing a while loop */
    type While <: Term

    given TypeTest[Tree, While] = WhileTypeTest
    protected val WhileTypeTest: TypeTest[Tree, While]

    val While: WhileModule

    trait WhileModule { this: While.type =>

      /** Creates a while loop `while (<cond>) <body>` and returns (<cond>, <body>) */
      def apply(cond: Term, body: Term): While

      def copy(original: Tree)(cond: Term, body: Term): While

      /** Extractor for while loops. Matches `while (<cond>) <body>` and returns (<cond>, <body>) */
      def unapply(x: While): Option[(Term, Term)]
    }

    given WhileMethods as WhileMethods = WhileMethodsImpl
    protected val WhileMethodsImpl: WhileMethods

    trait WhileMethods:
      extension (self: While):
        def cond: Term
        def body: Term
      end extension
    end WhileMethods

    // ----- TypeTrees ------------------------------------------------

    /** Type tree representing a type written in the source */
    type TypeTree <: Tree

    given TypeTest[Tree, TypeTree] = TypeTreeTypeTest
    protected val TypeTreeTypeTest: TypeTest[Tree, TypeTree]

    val TypeTree: TypeTreeModule

    trait TypeTreeModule { this: TypeTree.type =>
      /** Returns the tree of type or kind (TypeTree) of T */
      def of[T <: AnyKind](using Type[T]): TypeTree
    }

    given TypeTreeMethods as TypeTreeMethods = TypeTreeMethodsImpl
    protected val TypeTreeMethodsImpl: TypeTreeMethods

    trait TypeTreeMethods:
      extension (self: TypeTree):
        /** TypeRepr of this type tree */
        def tpe: TypeRepr
      end extension
    end TypeTreeMethods

    /** Type tree representing an inferred type */
    type Inferred <: TypeTree

    given TypeTest[Tree, Inferred] = InferredTypeTest
    protected val InferredTypeTest: TypeTest[Tree, Inferred]

    /** TypeTree containing an inferred type */
    val Inferred: InferredModule

    trait InferredModule { this: Inferred.type =>
      def apply(tpe: TypeRepr): Inferred
      /** Matches a TypeTree containing an inferred type */
      def unapply(x: Inferred): Boolean
    }

    /** Type tree representing a reference to definition with a given name */
    type TypeIdent <: TypeTree

    given TypeTest[Tree, TypeIdent] = TypeIdentTypeTest
    protected val TypeIdentTypeTest: TypeTest[Tree, TypeIdent]

    val TypeIdent: TypeIdentModule

    trait TypeIdentModule { this: TypeIdent.type =>
      def apply(sym: Symbol): TypeTree
      def copy(original: Tree)(name: String): TypeIdent
      def unapply(x: TypeIdent): Option[String]
    }

    given TypeIdentMethods as TypeIdentMethods = TypeIdentMethodsImpl
    protected val TypeIdentMethodsImpl: TypeIdentMethods

    trait TypeIdentMethods:
      extension (self: TypeIdent):
        def name: String
      end extension
    end TypeIdentMethods

    /** Type tree representing a selection of definition with a given name on a given term prefix */
    type TypeSelect <: TypeTree

    given TypeTest[Tree, TypeSelect] = TypeSelectTypeTest
    protected val TypeSelectTypeTest: TypeTest[Tree, TypeSelect]

    val TypeSelect: TypeSelectModule

    trait TypeSelectModule { this: TypeSelect.type =>
      def apply(qualifier: Term, name: String): TypeSelect
      def copy(original: Tree)(qualifier: Term, name: String): TypeSelect
      def unapply(x: TypeSelect): Option[(Term, String)]
    }

    given TypeSelectMethods as TypeSelectMethods = TypeSelectMethodsImpl
    protected val TypeSelectMethodsImpl: TypeSelectMethods

    trait TypeSelectMethods:
      extension (self: TypeSelect):
        def qualifier: Term
        def name: String
      end extension
    end TypeSelectMethods

    /** Type tree representing a selection of definition with a given name on a given type prefix */
    type TypeProjection <: TypeTree

    given TypeTest[Tree, TypeProjection] = TypeProjectionTypeTest
    protected val TypeProjectionTypeTest: TypeTest[Tree, TypeProjection]

    val TypeProjection: TypeProjectionModule

    trait TypeProjectionModule { this: TypeProjection.type =>
      // TODO def apply(qualifier: TypeTree, name: String): Project
      def copy(original: Tree)(qualifier: TypeTree, name: String): TypeProjection
      def unapply(x: TypeProjection): Option[(TypeTree, String)]
    }

    given TypeProjectionMethods as TypeProjectionMethods = TypeProjectionMethodsImpl
    protected val TypeProjectionMethodsImpl: TypeProjectionMethods

    trait TypeProjectionMethods:
      extension (self: TypeProjection):
        def qualifier: TypeTree
        def name: String
      end extension
    end TypeProjectionMethods

    /** Type tree representing a singleton type */
    type Singleton <: TypeTree

    given TypeTest[Tree, Singleton] = SingletonTypeTest
    protected val SingletonTypeTest: TypeTest[Tree, Singleton]

    val Singleton: SingletonModule

    trait SingletonModule { this: Singleton.type =>
      def apply(ref: Term): Singleton
      def copy(original: Tree)(ref: Term): Singleton
      def unapply(x: Singleton): Option[Term]
    }

    given SingletonMethods as SingletonMethods = SingletonMethodsImpl
    protected val SingletonMethodsImpl: SingletonMethods

    trait SingletonMethods:
      extension (self: Singleton):
        def ref: Term
      end extension
    end SingletonMethods

    /** Type tree representing a type refinement */
    type Refined <: TypeTree

    given TypeTest[Tree, Refined] = RefinedTypeTest
    protected val RefinedTypeTest: TypeTest[Tree, Refined]

    val Refined: RefinedModule

    trait RefinedModule { this: Refined.type =>
      // TODO def apply(tpt: TypeTree, refinements: List[Definition]): Refined
      def copy(original: Tree)(tpt: TypeTree, refinements: List[Definition]): Refined
      def unapply(x: Refined): Option[(TypeTree, List[Definition])]
    }

    given RefinedMethods as RefinedMethods = RefinedMethodsImpl
    protected val RefinedMethodsImpl: RefinedMethods

    trait RefinedMethods:
      extension (self: Refined):
        def tpt: TypeTree
        def refinements: List[Definition]
      end extension
    end RefinedMethods

    /** Type tree representing a type application */
    type Applied <: TypeTree

    given TypeTest[Tree, Applied] = AppliedTypeTest
    protected val AppliedTypeTest: TypeTest[Tree, Applied]

    val Applied: AppliedModule

    trait AppliedModule { this: Applied.type =>
      def apply(tpt: TypeTree, args: List[Tree /*TypeTree | TypeBoundsTree*/]): Applied
      def copy(original: Tree)(tpt: TypeTree, args: List[Tree /*TypeTree | TypeBoundsTree*/]): Applied
      def unapply(x: Applied): Option[(TypeTree, List[Tree /*TypeTree | TypeBoundsTree*/])]
    }

    given AppliedMethods as AppliedMethods = AppliedMethodsImpl
    protected val AppliedMethodsImpl: AppliedMethods

    trait AppliedMethods:
      extension (self: Applied):
        def tpt: TypeTree
        def args: List[Tree /*TypeTree | TypeBoundsTree*/]
      end extension
    end AppliedMethods

    /** Type tree representing an annotated type */
    type Annotated <: TypeTree

    given TypeTest[Tree, Annotated] = AnnotatedTypeTest
    protected val AnnotatedTypeTest: TypeTest[Tree, Annotated]

    val Annotated: AnnotatedModule

    trait AnnotatedModule { this: Annotated.type =>
      def apply(arg: TypeTree, annotation: Term): Annotated
      def copy(original: Tree)(arg: TypeTree, annotation: Term): Annotated
      def unapply(x: Annotated): Option[(TypeTree, Term)]
    }

    given AnnotatedMethods as AnnotatedMethods = AnnotatedMethodsImpl
    protected val AnnotatedMethodsImpl: AnnotatedMethods

    trait AnnotatedMethods:
      extension (self: Annotated):
        def arg: TypeTree
        def annotation: Term
      end extension
    end AnnotatedMethods

    /** Type tree representing a type match */
    type MatchTypeTree <: TypeTree

    given TypeTest[Tree, MatchTypeTree] = MatchTypeTreeTypeTest
    protected val MatchTypeTreeTypeTest: TypeTest[Tree, MatchTypeTree]

    val MatchTypeTree: MatchTypeTreeModule

    trait MatchTypeTreeModule { this: MatchTypeTree.type =>
      def apply(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef]): MatchTypeTree
      def copy(original: Tree)(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef]): MatchTypeTree
      def unapply(x: MatchTypeTree): Option[(Option[TypeTree], TypeTree, List[TypeCaseDef])]
    }

    given MatchTypeTreeMethods as MatchTypeTreeMethods = MatchTypeTreeMethodsImpl
    protected val MatchTypeTreeMethodsImpl: MatchTypeTreeMethods

    trait MatchTypeTreeMethods:
      extension (self: MatchTypeTree):
        def bound: Option[TypeTree]
        def selector: TypeTree
        def cases: List[TypeCaseDef]
      end extension
    end MatchTypeTreeMethods

    /** Type tree representing a by name parameter */
    type ByName <: TypeTree

    given TypeTest[Tree, ByName] = ByNameTypeTest
    protected val ByNameTypeTest: TypeTest[Tree, ByName]

    val ByName: ByNameModule

    trait ByNameModule { this: ByName.type =>
      def apply(result: TypeTree): ByName
      def copy(original: Tree)(result: TypeTree): ByName
      def unapply(x: ByName): Option[TypeTree]
    }

    given ByNameMethods as ByNameMethods = ByNameMethodsImpl
    protected val ByNameMethodsImpl: ByNameMethods

    trait ByNameMethods:
      extension (self: ByName):
        def result: TypeTree
      end extension
    end ByNameMethods

    /** Type tree representing a lambda abstraction type */
    type LambdaTypeTree <: TypeTree

    given TypeTest[Tree, LambdaTypeTree] = LambdaTypeTreeTypeTest
    protected val LambdaTypeTreeTypeTest: TypeTest[Tree, LambdaTypeTree]

    val LambdaTypeTree: LambdaTypeTreeModule

    trait LambdaTypeTreeModule { this: LambdaTypeTree.type =>
      def apply(tparams: List[TypeDef], body: Tree /*TypeTree | TypeBoundsTree*/): LambdaTypeTree
      def copy(original: Tree)(tparams: List[TypeDef], body: Tree /*TypeTree | TypeBoundsTree*/): LambdaTypeTree
      def unapply(tree: LambdaTypeTree): Option[(List[TypeDef], Tree /*TypeTree | TypeBoundsTree*/)]
    }

    given LambdaTypeTreeMethods as LambdaTypeTreeMethods = LambdaTypeTreeMethodsImpl
    protected val LambdaTypeTreeMethodsImpl: LambdaTypeTreeMethods

    trait LambdaTypeTreeMethods:
      extension (self: LambdaTypeTree):
        def tparams: List[TypeDef]
        def body: Tree /*TypeTree | TypeBoundsTree*/
      end extension
    end LambdaTypeTreeMethods

    /** Type tree representing a type binding */
    type TypeBind <: TypeTree

    given TypeTest[Tree, TypeBind] = TypeBindTypeTest
    protected val TypeBindTypeTest: TypeTest[Tree, TypeBind]

    val TypeBind: TypeBindModule

    trait TypeBindModule { this: TypeBind.type =>
      // TODO def apply(name: String, tree: Tree): TypeBind
      def copy(original: Tree)(name: String, tpt: Tree /*TypeTree | TypeBoundsTree*/): TypeBind
      def unapply(x: TypeBind): Option[(String, Tree /*TypeTree | TypeBoundsTree*/)]
    }

    given TypeBindMethods as TypeBindMethods = TypeBindMethodsImpl
    protected val TypeBindMethodsImpl: TypeBindMethods

    trait TypeBindMethods:
      extension (self: TypeBind):
        def name: String
        def body: Tree /*TypeTree | TypeBoundsTree*/
      end extension
    end TypeBindMethods

    /** Type tree within a block with aliases `{ type U1 = ... ; T[U1, U2] }` */
    type TypeBlock <: TypeTree

    given TypeTest[Tree, TypeBlock] = TypeBlockTypeTest
    protected val TypeBlockTypeTest: TypeTest[Tree, TypeBlock]

    val TypeBlock: TypeBlockModule

    trait TypeBlockModule { this: TypeBlock.type =>
      def apply(aliases: List[TypeDef], tpt: TypeTree): TypeBlock
      def copy(original: Tree)(aliases: List[TypeDef], tpt: TypeTree): TypeBlock
      def unapply(x: TypeBlock): Option[(List[TypeDef], TypeTree)]
    }

    given TypeBlockMethods as TypeBlockMethods = TypeBlockMethodsImpl
    protected val TypeBlockMethodsImpl: TypeBlockMethods

    trait TypeBlockMethods:
      extension (self: TypeBlock):
        def aliases: List[TypeDef]
        def tpt: TypeTree
      end extension
    end TypeBlockMethods

    // ----- TypeBoundsTrees ------------------------------------------------

    /** Type tree representing a type bound written in the source */
    type TypeBoundsTree <: Tree /*TypeTree | TypeBoundsTree*/

    given TypeTest[Tree, TypeBoundsTree] = TypeBoundsTreeTypeTest
    protected val TypeBoundsTreeTypeTest: TypeTest[Tree, TypeBoundsTree]

    val TypeBoundsTree: TypeBoundsTreeModule

    trait TypeBoundsTreeModule { this: TypeBoundsTree.type =>
      def apply(low: TypeTree, hi: TypeTree): TypeBoundsTree
      def copy(original: Tree)(low: TypeTree, hi: TypeTree): TypeBoundsTree
      def unapply(x: TypeBoundsTree): Option[(TypeTree, TypeTree)]
    }

    given TypeBoundsTreeMethods as TypeBoundsTreeMethods = TypeBoundsTreeMethodsImpl
    protected val TypeBoundsTreeMethodsImpl: TypeBoundsTreeMethods

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

    given TypeTest[Tree, WildcardTypeTree] = WildcardTypeTreeTypeTest
    protected val WildcardTypeTreeTypeTest: TypeTest[Tree, WildcardTypeTree]

    val WildcardTypeTree: WildcardTypeTreeModule

    trait WildcardTypeTreeModule { this: WildcardTypeTree.type =>
      def apply(tpe: TypeRepr): WildcardTypeTree
      /** Matches a TypeBoundsTree containing wildcard type bounds */
      def unapply(x: WildcardTypeTree): Boolean
    }

    given WildcardTypeTreeMethods as WildcardTypeTreeMethods = WildcardTypeTreeMethodsImpl
    protected val WildcardTypeTreeMethodsImpl: WildcardTypeTreeMethods

    trait WildcardTypeTreeMethods:
      extension (self: WildcardTypeTree):
        def tpe: TypeRepr
      end extension
    end WildcardTypeTreeMethods

    // ----- CaseDefs ------------------------------------------------

    /** Branch of a pattern match or catch clause */
    type CaseDef <: Tree

    given TypeTest[Tree, CaseDef] = CaseDefTypeTest
    protected val CaseDefTypeTest: TypeTest[Tree, CaseDef]

    val CaseDef: CaseDefModule

    trait CaseDefModule { this: CaseDef.type =>
      def apply(pattern: Tree, guard: Option[Term], rhs: Term): CaseDef
      def copy(original: Tree)(pattern: Tree, guard: Option[Term], rhs: Term): CaseDef
      def unapply(x: CaseDef): Option[(Tree, Option[Term], Term)]
    }

    given CaseDefMethods as CaseDefMethods = CaseDefMethodsImpl
    protected val CaseDefMethodsImpl: CaseDefMethods

    trait CaseDefMethods:
      extension (self: CaseDef):
        def pattern: Tree
        def guard: Option[Term]
        def rhs: Term
      end extension
    end CaseDefMethods

    /** Branch of a type pattern match */
    type TypeCaseDef <: Tree

    given TypeTest[Tree, TypeCaseDef] = TypeCaseDefTypeTest
    protected val TypeCaseDefTypeTest: TypeTest[Tree, TypeCaseDef]

    val TypeCaseDef: TypeCaseDefModule

    trait TypeCaseDefModule { this: TypeCaseDef.type =>
      def apply(pattern: TypeTree, rhs: TypeTree): TypeCaseDef
      def copy(original: Tree)(pattern: TypeTree, rhs: TypeTree): TypeCaseDef
      def unapply(tree: TypeCaseDef): Option[(TypeTree, TypeTree)]
    }

    given TypeCaseDefMethods as TypeCaseDefMethods = TypeCaseDefMethodsImpl
    protected val TypeCaseDefMethodsImpl: TypeCaseDefMethods

    trait TypeCaseDefMethods:
      extension (self: TypeCaseDef):
        def pattern: TypeTree
        def rhs: TypeTree
      end extension
    end TypeCaseDefMethods

    // ----- Patterns ------------------------------------------------

    /** Pattern representing a `_ @ _` binding. */
    type Bind <: Tree

    given TypeTest[Tree, Bind] = BindTypeTest
    protected val BindTypeTest: TypeTest[Tree, Bind]

    val Bind: BindModule

    trait BindModule { this: Bind.type =>
      def apply(sym: Symbol, pattern: Tree): Bind
      def copy(original: Tree)(name: String, pattern: Tree): Bind
      def unapply(pattern: Bind): Option[(String, Tree)]
    }

    given BindMethods as BindMethods = BindMethodsImpl
    protected val BindMethodsImpl: BindMethods

    trait BindMethods:
      extension (self: Bind):
        def name: String
        def pattern: Tree
      end extension
    end BindMethods

    /** Pattern representing a `Xyz(...)` unapply. */
    type Unapply <: Tree

    given TypeTest[Tree, Unapply] = UnapplyTypeTest
    protected val UnapplyTypeTest: TypeTest[Tree, Unapply]

    val Unapply: UnapplyModule

    trait UnapplyModule { this: Unapply.type =>
      // TODO def apply(fun: Term, implicits: List[Term], patterns: List[Tree]): Unapply
      def copy(original: Tree)(fun: Term, implicits: List[Term], patterns: List[Tree]): Unapply
      def unapply(x: Unapply): Option[(Term, List[Term], List[Tree])]
    }

    given UnapplyMethods as UnapplyMethods = UnapplyMethodsImpl
    protected val UnapplyMethodsImpl: UnapplyMethods

    trait UnapplyMethods:
      extension (self: Unapply):
        def fun: Term
        def implicits: List[Term]
        def patterns: List[Tree]
      end extension
    end UnapplyMethods

    /** Pattern representing `X | Y | ...` alternatives. */
    type Alternatives <: Tree

    given TypeTest[Tree, Alternatives] = AlternativesTypeTest
    protected val AlternativesTypeTest: TypeTest[Tree, Alternatives]

    val Alternatives: AlternativesModule

    trait AlternativesModule { this: Alternatives.type =>
      def apply(patterns: List[Tree]): Alternatives
      def copy(original: Tree)(patterns: List[Tree]): Alternatives
      def unapply(x: Alternatives): Option[List[Tree]]
    }

    given AlternativesMethods as AlternativesMethods = AlternativesMethodsImpl
    protected val AlternativesMethodsImpl: AlternativesMethods

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

    val ImportSelector: ImportSelectorModule

    trait ImportSelectorModule { this: ImportSelector.type => }

    /** Simple import selector: `.bar` in `import foo.bar` */
    type SimpleSelector <: ImportSelector

    given TypeTest[ImportSelector, SimpleSelector] = SimpleSelectorTypeTest
    protected val SimpleSelectorTypeTest: TypeTest[ImportSelector, SimpleSelector]

    val SimpleSelector: SimpleSelectorModule

    trait SimpleSelectorModule { this: SimpleSelector.type =>
      def unapply(x: SimpleSelector): Option[String]
    }

    given SimpleSelectorMethods as SimpleSelectorMethods = SimpleSelectorMethodsImpl
    protected val SimpleSelectorMethodsImpl: SimpleSelectorMethods

    trait SimpleSelectorMethods:
      extension (self: SimpleSelector):
        def name: String
        def namePos: Position
      end extension
    end SimpleSelectorMethods

    /** Rename import selector: `.{bar => baz}` in `import foo.{bar => baz}` */
    type RenameSelector <: ImportSelector

    given TypeTest[ImportSelector, RenameSelector] = RenameSelectorTypeTest
    protected val RenameSelectorTypeTest: TypeTest[ImportSelector, RenameSelector]

    val RenameSelector: RenameSelectorModule

    trait RenameSelectorModule { this: RenameSelector.type =>
      def unapply(x: RenameSelector): Option[(String, String)]
    }

    given RenameSelectorMethods as RenameSelectorMethods = RenameSelectorMethodsImpl
    protected val RenameSelectorMethodsImpl: RenameSelectorMethods

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

    given TypeTest[ImportSelector, OmitSelector] = OmitSelectorTypeTest
    protected val OmitSelectorTypeTest: TypeTest[ImportSelector, OmitSelector]

    val OmitSelector: OmitSelectorModule

    trait OmitSelectorModule { this: OmitSelector.type =>
      def unapply(x: OmitSelector): Option[String]
    }

    given OmitSelectorMethods as OmitSelectorMethods = OmitSelectorMethodsImpl
    protected val OmitSelectorMethodsImpl: OmitSelectorMethods

    trait OmitSelectorMethods:
      extension (self: OmitSelector):
        def name: String
        def namePos: Position
    end OmitSelectorMethods

    /** Omit import selector: `.given`/`.{given T}` in `import foo.given`/`import foo.{given T}` */
    type GivenSelector <: ImportSelector

    given TypeTest[ImportSelector, GivenSelector] = GivenSelectorTypeTest
    protected val GivenSelectorTypeTest: TypeTest[ImportSelector, GivenSelector]

    val GivenSelector: GivenSelectorModule

    trait GivenSelectorModule { this: GivenSelector.type =>
      def unapply(x: GivenSelector): Option[Option[TypeTree]]
    }

    given GivenSelectorMethods as GivenSelectorMethods = GivenSelectorMethodsImpl
    protected val GivenSelectorMethodsImpl: GivenSelectorMethods

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

    val TypeRepr: TypeReprModule

    trait TypeReprModule { this: TypeRepr.type =>
      /** Returns the type or kind (TypeRepr) of T */
      def of[T <: AnyKind](using Type[T]): TypeRepr

      /** Returns the type constructor of the runtime (erased) class */
      def typeConstructorOf(clazz: Class[?]): TypeRepr
    }

    given TypeReprMethods as TypeReprMethods = TypeReprMethodsImpl
    protected val TypeReprMethodsImpl: TypeReprMethods

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

    given TypeTest[TypeRepr, ConstantType] = ConstantTypeTypeTest
    protected val ConstantTypeTypeTest: TypeTest[TypeRepr, ConstantType]

    val ConstantType: ConstantTypeModule

    trait ConstantTypeModule { this: ConstantType.type =>
      def apply(x : Constant): ConstantType
      def unapply(x: ConstantType): Option[Constant]
    }

    given ConstantTypeMethods as ConstantTypeMethods = ConstantTypeMethodsImpl
    protected val ConstantTypeMethodsImpl: ConstantTypeMethods

    trait ConstantTypeMethods:
      extension (self: ConstantType):
        def constant: Constant
      end extension
    end ConstantTypeMethods

    /** Type of a reference to a term symbol */
    type TermRef <: TypeRepr

    given TypeTest[TypeRepr, TermRef] = TermRefTypeTest
    protected val TermRefTypeTest: TypeTest[TypeRepr, TermRef]

    val TermRef: TermRefModule

    trait TermRefModule { this: TermRef.type =>
      def apply(qual: TypeRepr, name: String): TermRef
      def unapply(x: TermRef): Option[(TypeRepr, String)]
    }

    given TermRefMethods as TermRefMethods = TermRefMethodsImpl
    protected val TermRefMethodsImpl: TermRefMethods

    trait TermRefMethods:
      extension (self: TermRef):
        def qualifier: TypeRepr
        def name: String
      end extension
    end TermRefMethods

    /** Type of a reference to a type symbol */
    type TypeRef <: TypeRepr

    given TypeTest[TypeRepr, TypeRef] = TypeRefTypeTest
    protected val TypeRefTypeTest: TypeTest[TypeRepr, TypeRef]

    val TypeRef: TypeRefModule

    trait TypeRefModule { this: TypeRef.type =>
      def unapply(x: TypeRef): Option[(TypeRepr, String)]
    }

    given TypeRefMethods as TypeRefMethods = TypeRefMethodsImpl
    protected val TypeRefMethodsImpl: TypeRefMethods

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

    given TypeTest[TypeRepr, SuperType] = SuperTypeTypeTest
    protected val SuperTypeTypeTest: TypeTest[TypeRepr, SuperType]

    val SuperType: SuperTypeModule

    trait SuperTypeModule { this: SuperType.type =>
      def apply(thistpe: TypeRepr, supertpe: TypeRepr): SuperType
      def unapply(x: SuperType): Option[(TypeRepr, TypeRepr)]
    }

    given SuperTypeMethods as SuperTypeMethods = SuperTypeMethodsImpl
    protected val SuperTypeMethodsImpl: SuperTypeMethods

    trait SuperTypeMethods { this: SuperTypeMethods =>
      extension (self: SuperType):
        def thistpe: TypeRepr
        def supertpe: TypeRepr
      end extension
    }

    /** A type with a type refinement `T { type U }` */
    type Refinement <: TypeRepr

    given TypeTest[TypeRepr, Refinement] = RefinementTypeTest
    protected val RefinementTypeTest: TypeTest[TypeRepr, Refinement]

    val Refinement: RefinementModule

    trait RefinementModule { this: Refinement.type =>
      def apply(parent: TypeRepr, name: String, info: TypeRepr): Refinement
      def unapply(x: Refinement): Option[(TypeRepr, String, TypeRepr)]
    }

    given RefinementMethods as RefinementMethods = RefinementMethodsImpl
    protected val RefinementMethodsImpl: RefinementMethods

    trait RefinementMethods:
      extension (self: Refinement):
        def parent: TypeRepr
        def name: String
        def info: TypeRepr
      end extension
    end RefinementMethods

    /** A higher kinded type applied to some types `T[U]` */
    type AppliedType <: TypeRepr

    given TypeTest[TypeRepr, AppliedType] = AppliedTypeTypeTest
    protected val AppliedTypeTypeTest: TypeTest[TypeRepr, AppliedType]

    val AppliedType: AppliedTypeModule

    trait AppliedTypeModule { this: AppliedType.type =>
      def unapply(x: AppliedType): Option[(TypeRepr, List[TypeRepr])]
    }

    given AppliedTypeMethods as AppliedTypeMethods = AppliedTypeMethodsImpl
    protected val AppliedTypeMethodsImpl: AppliedTypeMethods

    trait AppliedTypeMethods:
      extension (self: AppliedType):
        def tycon: TypeRepr
        def args: List[TypeRepr]
      end extension
    end AppliedTypeMethods

    /** A type with an anottation `T @foo` */
    type AnnotatedType <: TypeRepr

    given TypeTest[TypeRepr, AnnotatedType] = AnnotatedTypeTypeTest
    protected val AnnotatedTypeTypeTest: TypeTest[TypeRepr, AnnotatedType]

    val AnnotatedType: AnnotatedTypeModule

    trait AnnotatedTypeModule { this: AnnotatedType.type =>
      def apply(underlying: TypeRepr, annot: Term): AnnotatedType
      def unapply(x: AnnotatedType): Option[(TypeRepr, Term)]
    }

    given AnnotatedTypeMethods as AnnotatedTypeMethods = AnnotatedTypeMethodsImpl
    protected val AnnotatedTypeMethodsImpl: AnnotatedTypeMethods

    trait AnnotatedTypeMethods:
      extension (self: AnnotatedType):
        def underlying: TypeRepr
        def annot: Term
      end extension
    end AnnotatedTypeMethods

    /** Intersection type `T & U` */
    type AndType <: TypeRepr

    given TypeTest[TypeRepr, AndType] = AndTypeTypeTest
    protected val AndTypeTypeTest: TypeTest[TypeRepr, AndType]

    val AndType: AndTypeModule

    trait AndTypeModule { this: AndType.type =>
      def apply(lhs: TypeRepr, rhs: TypeRepr): AndType
      def unapply(x: AndType): Option[(TypeRepr, TypeRepr)]
    }

    given AndTypeMethods as AndTypeMethods = AndTypeMethodsImpl
    protected val AndTypeMethodsImpl: AndTypeMethods

    trait AndTypeMethods:
      extension (self: AndType):
        def left: TypeRepr
        def right: TypeRepr
      end extension
    end AndTypeMethods

    /** Union type `T | U` */
    type OrType <: TypeRepr

    given TypeTest[TypeRepr, OrType] = OrTypeTypeTest
    protected val OrTypeTypeTest: TypeTest[TypeRepr, OrType]

    val OrType: OrTypeModule

    trait OrTypeModule { this: OrType.type =>
      def apply(lhs: TypeRepr, rhs: TypeRepr): OrType
      def unapply(x: OrType): Option[(TypeRepr, TypeRepr)]
    }

    given OrTypeMethods as OrTypeMethods = OrTypeMethodsImpl
    protected val OrTypeMethodsImpl: OrTypeMethods

    trait OrTypeMethods:
      extension (self: OrType):
        def left: TypeRepr
        def right: TypeRepr
      end extension
    end OrTypeMethods

    /** Type match `T match { case U => ... }` */
    type MatchType <: TypeRepr

    given TypeTest[TypeRepr, MatchType] = MatchTypeTypeTest
    protected val MatchTypeTypeTest: TypeTest[TypeRepr, MatchType]

    val MatchType: MatchTypeModule

    trait MatchTypeModule { this: MatchType.type =>
      def apply(bound: TypeRepr, scrutinee: TypeRepr, cases: List[TypeRepr]): MatchType
      def unapply(x: MatchType): Option[(TypeRepr, TypeRepr, List[TypeRepr])]
    }

    given MatchTypeMethods as MatchTypeMethods = MatchTypeMethodsImpl
    protected val MatchTypeMethodsImpl: MatchTypeMethods

    trait MatchTypeMethods:
      extension (self: MatchType):
        def bound: TypeRepr
        def scrutinee: TypeRepr
        def cases: List[TypeRepr]
      end extension
    end MatchTypeMethods

    /** Type of a by by name parameter */
    type ByNameType <: TypeRepr

    given TypeTest[TypeRepr, ByNameType] = ByNameTypeTypeTest
    protected val ByNameTypeTypeTest: TypeTest[TypeRepr, ByNameType]

    val ByNameType: ByNameTypeModule

    trait ByNameTypeModule { this: ByNameType.type =>
      def apply(underlying: TypeRepr): TypeRepr
      def unapply(x: ByNameType): Option[TypeRepr]
    }

    given ByNameTypeMethods as ByNameTypeMethods = ByNameTypeMethodsImpl
    protected val ByNameTypeMethodsImpl: ByNameTypeMethods

    trait ByNameTypeMethods:
      extension (self: ByNameType):
        def underlying: TypeRepr
      end extension
    end ByNameTypeMethods

    /** Type of a parameter reference */
    type ParamRef <: TypeRepr

    given TypeTest[TypeRepr, ParamRef] = ParamRefTypeTest
    protected val ParamRefTypeTest: TypeTest[TypeRepr, ParamRef]

    val ParamRef: ParamRefModule

    trait ParamRefModule { this: ParamRef.type =>
      def unapply(x: ParamRef): Option[(LambdaType, Int)]
    }

    given ParamRefMethods as ParamRefMethods = ParamRefMethodsImpl
    protected val ParamRefMethodsImpl: ParamRefMethods

    trait ParamRefMethods:
      extension (self: ParamRef):
        def binder: LambdaType
        def paramNum: Int
      end extension
    end ParamRefMethods

    /** Type of `this` */
    type ThisType <: TypeRepr

    given TypeTest[TypeRepr, ThisType] = ThisTypeTypeTest
    protected val ThisTypeTypeTest: TypeTest[TypeRepr, ThisType]

    val ThisType: ThisTypeModule

    trait ThisTypeModule { this: ThisType.type =>
      def unapply(x: ThisType): Option[TypeRepr]
    }

    given ThisTypeMethods as ThisTypeMethods = ThisTypeMethodsImpl
    protected val ThisTypeMethodsImpl: ThisTypeMethods

    trait ThisTypeMethods:
      extension (self: ThisType):
        def tref: TypeRepr
      end extension
    end ThisTypeMethods

    /** A type that is recursively defined `this` */
    type RecursiveThis <: TypeRepr

    given TypeTest[TypeRepr, RecursiveThis] = RecursiveThisTypeTest
    protected val RecursiveThisTypeTest: TypeTest[TypeRepr, RecursiveThis]

    val RecursiveThis: RecursiveThisModule

    trait RecursiveThisModule { this: RecursiveThis.type =>
      def unapply(x: RecursiveThis): Option[RecursiveType]
    }

    given RecursiveThisMethods as RecursiveThisMethods = RecursiveThisMethodsImpl
    protected val RecursiveThisMethodsImpl: RecursiveThisMethods

    trait RecursiveThisMethods:
      extension (self: RecursiveThis):
        def binder: RecursiveType
      end extension
    end RecursiveThisMethods

    /** A type that is recursively defined */
    type RecursiveType <: TypeRepr

    given TypeTest[TypeRepr, RecursiveType] = RecursiveTypeTypeTest
    protected val RecursiveTypeTypeTest: TypeTest[TypeRepr, RecursiveType]

    val RecursiveType: RecursiveTypeModule

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

    given RecursiveTypeMethods as RecursiveTypeMethods = RecursiveTypeMethodsImpl
    protected val RecursiveTypeMethodsImpl: RecursiveTypeMethods

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

    given TypeTest[TypeRepr, MethodType] = MethodTypeTypeTest
    protected val MethodTypeTypeTest: TypeTest[TypeRepr, MethodType]

    val MethodType: MethodTypeModule

    trait MethodTypeModule { this: MethodType.type =>
      def apply(paramNames: List[String])(paramInfosExp: MethodType => List[TypeRepr], resultTypeExp: MethodType => TypeRepr): MethodType
      def unapply(x: MethodType): Option[(List[String], List[TypeRepr], TypeRepr)]
    }

    given MethodTypeMethods as MethodTypeMethods = MethodTypeMethodsImpl
    protected val MethodTypeMethodsImpl: MethodTypeMethods

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

    given TypeTest[TypeRepr, PolyType] = PolyTypeTypeTest
    protected val PolyTypeTypeTest: TypeTest[TypeRepr, PolyType]

    val PolyType: PolyTypeModule

    trait PolyTypeModule { this: PolyType.type =>
      def apply(paramNames: List[String])(paramBoundsExp: PolyType => List[TypeBounds], resultTypeExp: PolyType => TypeRepr): PolyType
      def unapply(x: PolyType): Option[(List[String], List[TypeBounds], TypeRepr)]
    }

    given PolyTypeMethods as PolyTypeMethods = PolyTypeMethodsImpl
    protected val PolyTypeMethodsImpl: PolyTypeMethods

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

    given TypeTest[TypeRepr, TypeLambda] = TypeLambdaTypeTest
    protected val TypeLambdaTypeTest: TypeTest[TypeRepr, TypeLambda]

    val TypeLambda: TypeLambdaModule

    trait TypeLambdaModule { this: TypeLambda.type =>
      def apply(paramNames: List[String], boundsFn: TypeLambda => List[TypeBounds], bodyFn: TypeLambda => TypeRepr): TypeLambda
      def unapply(x: TypeLambda): Option[(List[String], List[TypeBounds], TypeRepr)]
    }

    given TypeLambdaMethods as TypeLambdaMethods = TypeLambdaMethodsImpl
    protected val TypeLambdaMethodsImpl: TypeLambdaMethods

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

    given TypeTest[TypeRepr, TypeBounds] = TypeBoundsTypeTest
    protected val TypeBoundsTypeTest: TypeTest[TypeRepr, TypeBounds]

    val TypeBounds: TypeBoundsModule

    trait TypeBoundsModule { this: TypeBounds.type =>
      def apply(low: TypeRepr, hi: TypeRepr): TypeBounds
      def unapply(x: TypeBounds): Option[(TypeRepr, TypeRepr)]
      def empty: TypeBounds
      def upper(hi: TypeRepr): TypeBounds
      def lower(lo: TypeRepr): TypeBounds
    }

    given TypeBoundsMethods as TypeBoundsMethods = TypeBoundsMethodsImpl
    protected val TypeBoundsMethodsImpl: TypeBoundsMethods

    trait TypeBoundsMethods:
      extension (self: TypeBounds):
        def low: TypeRepr
        def hi: TypeRepr
      end extension
    end TypeBoundsMethods

    // ----- NoPrefix -------------------------------------------------

    /** NoPrefix for a type selection */
    type NoPrefix <: TypeRepr

    given TypeTest[TypeRepr, NoPrefix] = NoPrefixTypeTest
    protected val NoPrefixTypeTest: TypeTest[TypeRepr, NoPrefix]

    val NoPrefix: NoPrefixModule

    trait NoPrefixModule { this: NoPrefix.type =>
      def unapply(x: NoPrefix): Boolean
    }

    ///////////////
    // CONSTANTS //
    ///////////////

    /** Constant value represented as the constant itself */
    type Constant <: AnyRef

    /** Constant value represented as the constant itself */
    val Constant: ConstantModule

    /** Constant value represented as the constant itself */
    trait ConstantModule { this: Constant.type =>

      /** Constant Boolean value */
      val Boolean: ConstantBooleanModule

      /** Constant Boolean value */
      trait ConstantBooleanModule { this: Boolean.type =>
        /** Create a constant Boolean value */
        def apply(x: Boolean): Constant
        /** Match Boolean value constant and extract its value */
        def unapply(constant: Constant): Option[Boolean]
      }

      /** Constant Byte value */
      val Byte: ConstantByteModule

      /** Constant Byte value */
      trait ConstantByteModule { this: Byte.type =>
        /** Create a constant Byte value */
        def apply(x: Byte): Constant
        /** Match Byte value constant and extract its value */
        def unapply(constant: Constant): Option[Byte]
      }

      /** Constant Short value */
      val Short: ConstantShortModule

      /** Constant Short value */
      trait ConstantShortModule { this: Short.type =>
        /** Create a constant Short value */
        def apply(x: Short): Constant
        /** Match Short value constant and extract its value */
        def unapply(constant: Constant): Option[Short]
      }

      /** Constant Int value */
      val Int: ConstantIntModule

      /** Constant Int value */
      trait ConstantIntModule { this: Int.type =>
        /** Create a constant Int value */
        def apply(x: Int): Constant
        /** Match Int value constant and extract its value */
        def unapply(constant: Constant): Option[Int]
      }

      /** Constant Long value */
      val Long: ConstantLongModule

      /** Constant Long value */
      trait ConstantLongModule { this: Long.type =>
        /** Create a constant Long value */
        def apply(x: Long): Constant
        /** Match Long value constant and extract its value */
        def unapply(constant: Constant): Option[Long]
      }

      /** Constant Float value */
      val Float: ConstantFloatModule

      /** Constant Float value */
      trait ConstantFloatModule { this: Float.type =>
        /** Create a constant Float value */
        def apply(x: Float): Constant
        /** Match Float value constant and extract its value */
        def unapply(constant: Constant): Option[Float]
      }

      /** Constant Double value */
      val Double: ConstantDoubleModule

      /** Constant Double value */
      trait ConstantDoubleModule { this: Double.type =>
        /** Create a constant Double value */
        def apply(x: Double): Constant
        /** Match Double value constant and extract its value */
        def unapply(constant: Constant): Option[Double]
      }

      /** Constant Char value */
      val Char: ConstantCharModule

      /** Constant Char value */
      trait ConstantCharModule { this: Char.type =>
        /** Create a constant Char value */
        def apply(x: Char): Constant
        /** Match Char value constant and extract its value */
        def unapply(constant: Constant): Option[Char]
      }

      /** Constant String value */
      val String: ConstantStringModule

      /** Constant String value */
      trait ConstantStringModule { this: String.type =>
        /** Create a constant String value */
        def apply(x: String): Constant
        /** Match String value constant and extract its value */
        def unapply(constant: Constant): Option[String]
      }

      /** Constant Unit value */
      val Unit: ConstantUnitModule

      /** Constant Unit value */
      trait ConstantUnitModule { this: Unit.type =>
        /** Create a constant Unit value */
        def apply(): Constant
        /** Match Unit value constant */
        def unapply(constant: Constant): Boolean
      }

      /** Constant null value */
      val Null: ConstantNullModule

      /** Constant null value */
      trait ConstantNullModule { this: Null.type =>
        /** Create a constant null value */
        def apply(): Constant
        /** Match null value constant */
        def unapply(constant: Constant): Boolean
      }

      /** Constant class value representing a `classOf[T]` */
      val ClassOf: ConstantClassOfModule

      /** Constant class value representing a `classOf[T]` */
      trait ConstantClassOfModule { this: ClassOf.type =>
        /** Create a constant class value representing `classOf[<tpe>]` */
        def apply(tpe: TypeRepr): Constant
        /** Match a class value constant representing `classOf[<tpe>]` and extract its type */
        def unapply(constant: Constant): Option[TypeRepr]
      }

    }

    given ConstantMethods as ConstantMethods = ConstantMethodsImpl
    protected val ConstantMethodsImpl: ConstantMethods

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

    val Implicits: ImplicitsModule

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

    given TypeTest[ImplicitSearchResult, ImplicitSearchSuccess] = ImplicitSearchSuccessTypeTest
    protected val ImplicitSearchSuccessTypeTest: TypeTest[ImplicitSearchResult, ImplicitSearchSuccess]

    type ImplicitSearchSuccess <: ImplicitSearchResult

    given ImplicitSearchSuccessMethods as ImplicitSearchSuccessMethods = ImplicitSearchSuccessMethodsImpl
    protected val ImplicitSearchSuccessMethodsImpl: ImplicitSearchSuccessMethods

    trait ImplicitSearchSuccessMethods:
      extension (self: ImplicitSearchSuccess):
        def tree: Term
      end extension
    end ImplicitSearchSuccessMethods

    type ImplicitSearchFailure <: ImplicitSearchResult

    given TypeTest[ImplicitSearchResult, ImplicitSearchFailure] = ImplicitSearchFailureTypeTest
    protected val ImplicitSearchFailureTypeTest: TypeTest[ImplicitSearchResult, ImplicitSearchFailure]

    given ImplicitSearchFailureMethods as ImplicitSearchFailureMethods = ImplicitSearchFailureMethodsImpl
    protected val ImplicitSearchFailureMethodsImpl: ImplicitSearchFailureMethods

    trait ImplicitSearchFailureMethods:
      extension (self: ImplicitSearchFailure):
        def explanation: String
      end extension
    end ImplicitSearchFailureMethods

    type DivergingImplicit <: ImplicitSearchFailure

    given TypeTest[ImplicitSearchResult, DivergingImplicit] = DivergingImplicitTypeTest
    protected val DivergingImplicitTypeTest: TypeTest[ImplicitSearchResult, DivergingImplicit]

    type NoMatchingImplicits <: ImplicitSearchFailure

    given TypeTest[ImplicitSearchResult, NoMatchingImplicits] = NoMatchingImplicitsTypeTest
    protected val NoMatchingImplicitsTypeTest: TypeTest[ImplicitSearchResult, NoMatchingImplicits]

    type AmbiguousImplicits <: ImplicitSearchFailure

    given TypeTest[ImplicitSearchResult, AmbiguousImplicits] = AmbiguousImplicitsTypeTest
    protected val AmbiguousImplicitsTypeTest: TypeTest[ImplicitSearchResult, AmbiguousImplicits]

    /////////////
    // SYMBOLS //
    /////////////

    /** Symbol of a definition.
    *  Then can be compared with == to know if the definition is the same.
    */
    type Symbol <: AnyRef

    val Symbol: SymbolModule

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

    given SymbolMethods as SymbolMethods = SymbolMethodsImpl
    protected val SymbolMethodsImpl: SymbolMethods

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

    /** The signature of a method */
    val Signature: SignatureModule

    trait SignatureModule { this: Signature.type =>
      /** Matches the method signature and returns its parameters and result type. */
      def unapply(sig: Signature): Option[(List[String | Int], String)]
    }

    given SignatureMethods as SignatureMethods = SignatureMethodsImpl
    protected val SignatureMethodsImpl: SignatureMethods

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
    val defn: DefnModule

    /** Defines standard symbols (and types via its base trait). */
    trait DefnModule { self: defn.type =>

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

    val Flags: FlagsModule

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

    given FlagsMethods as FlagsMethods = FlagsMethodsImpl
    protected val FlagsMethodsImpl: FlagsMethods

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

    val Position: PositionModule

    trait PositionModule { this: Position.type =>
      /** Position of the expansion site of the macro */
      def ofMacroExpansion: Position
    }

    given PositionMethods as PositionMethods = PositionMethodsImpl
    protected val PositionMethodsImpl: PositionMethods

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

    val SourceFile: SourceFileModule

    trait SourceFileModule { this: SourceFile.type => }

    given SourceFileMethods as SourceFileMethods = SourceFileMethodsImpl
    protected val SourceFileMethodsImpl: SourceFileMethods

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

    val Source: SourceModule

    trait SourceModule { this: Source.type =>

      /** Returns the source file being compiled. The path is relative to the current working directory. */
      def path: java.nio.file.Path

    }

    ///////////////
    // REPORTING //
    ///////////////

    val report: ReportModule

    /** Module containg error and waring reporiting. */
    trait ReportModule { self: report.type =>

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

    val Documentation: DocumentationModule

    trait DocumentationModule { this: Documentation.type => }

    given DocumentationMethods as DocumentationMethods = DocumentationMethodsImpl
    protected val DocumentationMethodsImpl: DocumentationMethods

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
