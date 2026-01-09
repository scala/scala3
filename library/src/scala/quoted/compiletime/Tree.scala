package scala.quoted.compiletime

import scala.quoted.Expr
import scala.quoted.Type

/////// Tree ///////////////////////////////////////////////////////////////

/** Tree representing code written in the source. */
sealed trait Tree private[compiletime] () {

  /** Position in the source code. */
  def pos: Position

  /** Symbol of defined or referred by this tree. */
  def symbol: Symbol

  /** Shows the tree as String. */
  def show(using Printer[Tree]): String

  /** Does this tree represent a valid expression? */
  def isExpr: Boolean

  /** Converts this tree to an `quoted.Expr[Any]` if the tree is a valid expression or throws. */
  def asExpr: Expr[Any]

  /** Converts this tree to an `quoted.Expr[T]` if the tree is a valid expression or throws. */
  def asExprOf[T](using Type[T]): Expr[T]

  /** Changes the owner of the symbols in the tree. */
  def changeOwner(newOwner: Symbol): Tree

}
object Tree {

  def quoted(using quotes: Quotes): Tree.Module = quotes.reflectV2.Tree
  given moduleConversion: (quotes: Quotes) => Conversion[Tree.type, Tree.Module] = _ => quotes.reflectV2.Tree

  trait Module private[compiletime] () {}

}

/////// PackageClause ///////////////////////////////////////////////////////////////

/**
  * Tree representing a package clause in the source code
  *
  *  ```scala sc:nocompile
  *  package foo {
  *    // package stats
  *  }
  *  ```
  *
  *  or
  *
  *  ```scala sc:nocompile
  *  package foo.bar
  *  // package stats
  *  ```
  */
trait PackageClause private[compiletime] () extends Tree {

  /** Tree containing the package name. */
  def pid: Ref

  /** Definitions, imports or exports within the package. */
  def stats: List[Tree]
}
object PackageClause {

  def quoted(using quotes: Quotes): PackageClause.Module = quotes.reflectV2.PackageClause
  given moduleConversion: (quotes: Quotes) => Conversion[PackageClause.type, PackageClause.Module] = _ => quotes.reflectV2.PackageClause

  def unapply(tree: PackageClause): (Ref, List[Tree]) = (tree.pid, tree.stats)

  trait Module private[compiletime] () {

    /** Creates a package clause `package pid { stats }`. */
    def apply(pid: Ref, stats: List[Tree]): PackageClause

    /** Creates a package clause `package pid { stats }`. */
    def make(pid: Ref, stats: List[Tree]): PackageClause
  }

}

/////// Statement ///////////////////////////////////////////////////////////////

/** Tree representing a statement in the source code. */
sealed trait Statement private[compiletime] () extends Tree
object Statement {

  def quoted(using quotes: Quotes): Statement.Module = quotes.reflectV2.Statement
  given moduleConversion: (quotes: Quotes) => Conversion[Statement.type, Statement.Module] = _ => quotes.reflectV2.Statement

  trait Module private[compiletime] () {}

}

/////// Import ///////////////////////////////////////////////////////////////

/**
  * Tree representing an import in the source code.
  *
  *  See also documentation on `Selector`.
  */
trait Import private[compiletime] () extends Statement {

  /** Qualifier of the import. */
  def expr: Term

  /**
    * List selectors of the import
    *
    *  See documentation on `Selector`
    */
  def selectors: List[Selector]
}
object Import {

  def quoted(using quotes: Quotes): Import.Module = quotes.reflectV2.Import
  given moduleConversion: (quotes: Quotes) => Conversion[Import.type, Import.Module] = _ => quotes.reflectV2.Import

  def unapply(tree: Import): (Term, List[Selector]) = (tree.expr, tree.selectors)

  trait Module private[compiletime] () {

    /** Creates an `Import` with the given qualifier and selectors. */
    def apply(expr: Term, selectors: List[Selector]): Import

    /** Creates an `Import` with the given qualifier and selectors. */
    def make(expr: Term, selectors: List[Selector]): Import
  }

}

/////// Export ///////////////////////////////////////////////////////////////

/**
  * Tree representing an export clause in the source code.
  *  Export forwarders generated from this clause appear in the same scope.
  */
trait Export private[compiletime] () extends Statement {

  /** Qualifier of the export. */
  def expr: Term

  /**
    * List selectors of the export
    *
    *  See documentation on `Selector`
    */
  def selectors: List[Selector]
}
object Export {

  def quoted(using quotes: Quotes): Export.Module = quotes.reflectV2.Export
  given moduleConversion: (quotes: Quotes) => Conversion[Export.type, Export.Module] = _ => quotes.reflectV2.Export

  def unapply(tree: Export): (Term, List[Selector]) = (tree.expr, tree.selectors)

  trait Module private[compiletime] () {}

}

/////// Definition ///////////////////////////////////////////////////////////////

/** Tree representing a definition in the source code. It can be `ClassDef`, `TypeDef`, `DefDef` or `ValDef`. */
sealed trait Definition private[compiletime] () extends Statement {

  /** Name of the definition. */
  def name: String
}
object Definition {

  def quoted(using quotes: Quotes): Definition.Module = quotes.reflectV2.Definition
  given moduleConversion: (quotes: Quotes) => Conversion[Definition.type, Definition.Module] = _ => quotes.reflectV2.Definition

  trait Module private[compiletime] () {}

}

/////// ClassDef ///////////////////////////////////////////////////////////////

/** Tree representing a class definition. This includes anonymous class definitions and the class of a module object. */
trait ClassDef private[compiletime] () extends Definition {

  /** The primary constructor of this class. */
  def constructor: DefDef

  /**
    * List of extended parent classes or traits.
    *  The first parent is always a class.
    */
  def parents: List[Tree]

  /**
    * Self-type of the class
    *
    *  ```scala
    *  //{
    *  type T
    *  //}
    *  class C { self: T =>
    *    ???
    *  }
    *  ```
    */
  def self: Option[ValDef]

  /**
    * Statements within the class
    *
    *  ```scala
    *  class C {
    *    ??? // statements
    *  }
    *  ```
    */
  def body: List[Statement]
}
object ClassDef {

  def quoted(using quotes: Quotes): ClassDef.Module = quotes.reflectV2.ClassDef
  given moduleConversion: (quotes: Quotes) => Conversion[ClassDef.type, ClassDef.Module] = _ => quotes.reflectV2.ClassDef

  def unapply(cdef: ClassDef): (String, DefDef, List[Tree], Option[ValDef], List[Statement]) = (cdef.name, cdef.constructor, cdef.parents, cdef.self, cdef.body)

  trait Module private[compiletime] () {

    /**
      * Creates a class definition tree
      *
      *  @param cls The class symbol. A new class symbol can be created using `Symbol.newClass`.
      *  @param parents The parents trees class. The trees must align with the parent types of `cls`.
      *                 Parents can be `TypeTree`s if they don't have term parameter,
      *                 otherwise the can be `Term` containing the `New` applied to the parameters of the extended class.
      *  @param body List of members of the class. The members must align with the members of `cls`.
      */
    def apply(cls: Symbol, parents: List[Tree], body: List[Statement]): ClassDef

    /** Creates a class definition tree */
    def make(cls: Symbol, parents: List[Tree], body: List[Statement]): ClassDef

    /**
      * Creates the ValDef and ClassDef of a module (equivalent to an `object` declaration in source code).
      *
      *  Equivalent to
      *  ```
      *  def module(module: Symbol, parents: List[Tree], body: List[Statement]): (ValDef, ClassDef) =
      *    val modCls = module.moduleClass
      *    val modClassDef = ClassDef(modCls, parents, body)
      *    val modValDef = ValDef(module, Some(Apply(Select(New(TypeIdent(modCls)), cls.primaryConstructor), Nil)))
      *    List(modValDef, modClassDef)
      *  ```
      *
      *  @param module the module symbol (created using `Symbol.newModule`)
      *  @param parents parents of the module class
      *  @param body body of the module class
      *  @return The module lazy val definition and module class definition.
      *          These should be added one after the other (in that order) in the body of a class or statements of a block.
      *
      *  @syntax markdown
      */
    def module(module: Symbol, parents: List[Tree], body: List[Statement]): (ValDef, ClassDef)
  }

}

/////// ValOrDefDef ///////////////////////////////////////////////////////////////

/**
  * Tree representing a value or method definition in the source code.
  *  This includes `def`, `val`, `lazy val`, `var`, `object` and parameter definitions.
  */
sealed trait ValOrDefDef private[compiletime] () extends Definition {

  /** The type tree of this `val` or `def` definition. */
  def tpt: TypeTree

  /** The right-hand side of this `val` or `def` definition. */
  def rhs: Option[Term]
}
object ValOrDefDef {

  def quoted(using quotes: Quotes): ValOrDefDef.Module = quotes.reflectV2.ValOrDefDef
  given moduleConversion: (quotes: Quotes) => Conversion[ValOrDefDef.type, ValOrDefDef.Module] = _ => quotes.reflectV2.ValOrDefDef

  trait Module private[compiletime] () {}

}

/////// DefDef ///////////////////////////////////////////////////////////////

/** Tree representing a method definition in the source code. */
trait DefDef private[compiletime] () extends ValOrDefDef {

  /** List of type and term parameter clauses. */
  def paramss: List[ParamClause]

  /**
    * List of leading type parameters or Nil if the method does not have leading type parameters.
    *
    *  Note: Non leading type parameters can be found in extension methods such as
    *  ```scala
    *  //{
    *  type A
    *  type T
    *  //}
    *  extension (a: A) def f[T]() = ???
    *  ```
    */
  def leadingTypeParams: List[TypeDef]

  /**
    * List of parameter clauses following the leading type parameters or all clauses.
    *  Returns all parameter clauses if there are no leading type parameters.
    *
    *  Non leading type parameters can be found in extension methods such as
    *  ```scala
    *  //{
    *  type T
    *  type A
    *  //}
    *  extension (a: A) def f[T]() = ???
    *  ```
    */
  def trailingParamss: List[ParamClause]

  /** List of term parameter clauses. */
  def termParamss: List[TermParamClause]

  /** The tree of the return type of this `def` definition. */
  def returnTpt: TypeTree
}
object DefDef {

  def quoted(using quotes: Quotes): DefDef.Module = quotes.reflectV2.DefDef
  given moduleConversion: (quotes: Quotes) => Conversion[DefDef.type, DefDef.Module] = _ => quotes.reflectV2.DefDef

  def unapply(ddef: DefDef): (String, List[ParamClause], TypeTree, Option[Term]) = (ddef.name, ddef.paramss, ddef.returnTpt, ddef.rhs)

  trait Module private[compiletime] () {

    /**
      * Creates a method definition `def f[..](...)` with the signature defined in the symbol.
      *
      *  The `rhsFn` is a function that receives references to its parameters, and should return
      *  `Some` containing the implementation of the method, or `None` if the method has no implementation.
      *  Any definition directly inside the implementation should have `symbol` as owner.
      *
      *  Use `Symbol.asQuotes` to create the rhs using quoted code.
      *
      *  See also: `Tree.changeOwner`
      */
    def apply(symbol: Symbol, rhsFn: List[List[Tree]] => Option[Term]): DefDef

    /** Creates a method definition `def f[..](...)` with the signature defined in the symbol. */
    def make(symbol: Symbol, rhsFn: List[List[Tree]] => Option[Term]): DefDef
  }

}

/////// ValDef ///////////////////////////////////////////////////////////////

/** Tree representing a value definition in the source code. This includes `val`, `lazy val`, `var`, `object` and parameter definitions. */
trait ValDef private[compiletime] () extends ValOrDefDef
object ValDef {

  def quoted(using quotes: Quotes): ValDef.Module = quotes.reflectV2.ValDef
  given moduleConversion: (quotes: Quotes) => Conversion[ValDef.type, ValDef.Module] = _ => quotes.reflectV2.ValDef

  def unapply(vdef: ValDef): (String, TypeTree, Option[Term]) = (vdef.name, vdef.tpt, vdef.rhs)

  trait Module private[compiletime] () {

    /**
      * Creates a value definition `val x`, `var x` or `lazy val x` with the signature defined in the symbol.
      *
      *  The `rhs` should return `Some` containing the implementation of the method,
      *  or `None` if the method has no implementation.
      *  Any definition directly inside the implementation should have `symbol` as owner.
      *
      *  Use `Symbol.asQuotes` to create the rhs using quoted code.
      *
      *  See also: `Tree.changeOwner`
      */
    def apply(symbol: Symbol, rhs: Option[Term]): ValDef

    /** Creates a value definition `val x`, `var x` or `lazy val x` with the signature defined in the symbol. */
    def make(symbol: Symbol, rhs: Option[Term]): ValDef

    /**
      * Creates a block `{ val <name> = <rhs: Term>; <body(x): Term> }`
      *
      *  Usage:
      *  ```
      *  ValDef.let(owner, "x", rhs1, Flags.Lazy) { x =>
      *    ValDef.let(x.symbol.owner, "y", rhs2, Flags.Mutable) { y =>
      *      // use `x` and `y`
      *    }
      *  }
      *  ```
      *
      *  @param flags extra flags to with which the symbol should be constructed. Can be `Final | Implicit | Lazy | Mutable | Given | Synthetic`
      */
    def let(owner: Symbol, name: String, rhs: Term, flags: Flags)(body: Ref => Term): Term

    /**
      * Creates a block `{ val <name> = <rhs: Term>; <body(x): Term> }`
      *
      *  Usage:
      *  ```
      *  ValDef.let(owner, "x", rhs1) { x =>
      *    ValDef.let(x.symbol.owner, "y", rhs2) { y =>
      *      // use `x` and `y`
      *    }
      *  }
      *  ```
      */
    def let(owner: Symbol, name: String, rhs: Term)(body: Ref => Term): Term

    /**
      * Creates a block `{ val x = <rhs: Term>; <body(x): Term> }`
      *
      *  Usage:
      *  ```
      *  ValDef.let(owner, rhs1) { x =>
      *    ValDef.let(owner, rhs2) { y =>
      *      // use `x` and `y`
      *    }
      *  }
      *  ```
      */
    def let(owner: Symbol, rhs: Term)(body: Ref => Term): Term

    /**
      * Creates a block `{ val x1 = <terms(0): Term>; ...; val xn = <terms(n-1): Term>; <body(List(x1, ..., xn)): Term> }`
      *
      *  Usage:
      *  ```
      *  ValDef.let(owner, rhsList) { xs =>
      *     ...
      *  }
      *  ```
      */
    def let(owner: Symbol, terms: List[Term])(body: List[Ref] => Term): Term
  }

}

/////// TypeDef ///////////////////////////////////////////////////////////////

/** Tree representing a type (parameter or member) definition in the source code. */
trait TypeDef private[compiletime] () extends Definition {

  /** The type bounds on the right-hand side of this `type` definition. */
  def rhs: Tree
}
object TypeDef {

  def quoted(using quotes: Quotes): TypeDef.Module = quotes.reflectV2.TypeDef
  given moduleConversion: (quotes: Quotes) => Conversion[TypeDef.type, TypeDef.Module] = _ => quotes.reflectV2.TypeDef

  def unapply(tdef: TypeDef): (String, Tree) = (tdef.name, tdef.rhs)

  trait Module private[compiletime] () {
    def apply(symbol: Symbol): TypeDef
    def make(symbol: Symbol): TypeDef
  }

}

/////// Term ///////////////////////////////////////////////////////////////

/** Tree representing an expression in the source code. */
sealed trait Term private[compiletime] () extends Statement {

  /** TypeRepr of this term. */
  def tpe: TypeRepr

  /**
    * Replaces Inlined nodes and InlineProxy references to underlying arguments.
    *  The resulting tree is useful for inspection of the value or content of a non-inline argument.
    *
    *  Warning: This tree may contain references that are out of scope and should not be used in the generated code.
    *           This method should only used to port Scala 2 that used to access their outer scope unsoundly.
    */
  def underlyingArgument: Term

  /**
    * Replaces Ident nodes references to the underlying tree that defined them.
    *  The resulting tree is useful for inspection of the definition of some bindings.
    *
    *  Warning: This tree may contain references that are out of scope and should not be used in the generated code.
    *           This method should only used to port Scala 2 that used to access their outer scope unsoundly.
    */
  def underlying: Term

  /** Converts a partially applied term into a lambda expression. */
  def etaExpand(owner: Symbol): Term

  /** A unary apply node with given argument: `tree(arg)`. */
  def appliedTo(arg: Term): Term

  /** An apply node with given arguments: `tree(arg, args0, ..., argsN)`. */
  def appliedTo(arg: Term, args: Term*): Term

  /** An apply node with given argument list `tree(args(0), ..., args(args.length - 1))`. */
  def appliedToArgs(args: List[Term]): Apply

  /**
    * The current tree applied to given argument lists:
    *  `tree (argss(0)) ... (argss(argss.length -1))`
    */
  def appliedToArgss(argss: List[List[Term]]): Term

  /** The current tree applied to (): `tree()`. */
  def appliedToNone: Apply

  /** The current tree applied to `()` unless the tree's widened type is parameterless or expects type parameters. */
  def ensureApplied: Term

  /** The current tree applied to given type argument: `tree[targ]`. */
  def appliedToType(targ: TypeRepr): Term

  /** The current tree applied to given type arguments: `tree[targ0, ..., targN]`. */
  def appliedToTypes(targs: List[TypeRepr]): Term

  /** The current tree applied to given type argument list: `tree[targs(0), ..., targs(targs.length - 1)]`. */
  def appliedToTypeTrees(targs: List[TypeTree]): Term

  /** A select node that selects the given symbol. */
  def select(sym: Symbol): Select
}
object Term {

  def quoted(using quotes: Quotes): Term.Module = quotes.reflectV2.Term
  given moduleConversion: (quotes: Quotes) => Conversion[Term.type, Term.Module] = _ => quotes.reflectV2.Term

  trait Module private[compiletime] () {

    /**
      * Returns a term that is functionally equivalent to `t`,
      *  however if `t` is of the form `((y1, ..., yn) => e2)(e1, ..., en)`
      *  then it optimizes the top most call by returning `Some`
      *  with the result of beta-reducing the function application.
      *  Similarly, all outermost curried function applications will be beta-reduced, if possible.
      *  Otherwise returns `None`.
      *
      *  To retain semantics the argument `ei` is bound as `val yi = ei` and by-name arguments to `def yi = ei`.
      *  Some bindings may be elided as an early optimization.
      *
      *  Example:
      *  ```scala sc:nocompile
      *  ((a: Int, b: Int) => a + b).apply(x, y)
      *  ```
      *  will be reduced to
      *  ```scala sc:nocompile
      *  val a = x
      *  val b = y
      *  a + b
      *  ```
      *
      *  Generally:
      *  ```scala sc:nocompile
      *  ([X1, Y1, ...] => (x1, y1, ...) => ... => [Xn, Yn, ...] => (xn, yn, ...) => f[X1, Y1, ..., Xn, Yn, ...](x1, y1, ..., xn, yn, ...))).apply[Tx1, Ty1, ...](myX1, myY1, ...)....apply[Txn, Tyn, ...](myXn, myYn, ...)
      *  ```
      *  will be reduced to
      *  ```scala sc:nocompile
      *  type X1 = Tx1
      *  type Y1 = Ty1
      *  ...
      *  val x1 = myX1
      *  val y1 = myY1
      *  ...
      *  type Xn = Txn
      *  type Yn = Tyn
      *  ...
      *  val xn = myXn
      *  val yn = myYn
      *  ...
      *  f[X1, Y1, ..., Xn, Yn, ...](x1, y1, ..., xn, yn, ...)
      *  ```
      */
    def betaReduce(term: Term): Option[Term]
  }

}

/////// Ref ///////////////////////////////////////////////////////////////

/** Tree representing a reference to definition. */
sealed trait Ref private[compiletime] () extends Term
object Ref {

  def quoted(using quotes: Quotes): Ref.Module = quotes.reflectV2.Ref
  given moduleConversion: (quotes: Quotes) => Conversion[Ref.type, Ref.Module] = _ => quotes.reflectV2.Ref

  trait Module private[compiletime] () {

    /** A tree representing the same reference as the given type. */
    def term(tp: TermRef): Ref

    /**
      * Creates a reference tree from a symbol
      *
      *  If `sym` refers to a class member `foo` in class `C`,
      *  returns a tree representing `C.this.foo`.
      *
      *  If `sym` refers to an object member `foo` in object C, itself in prefix
      *  `pre` (which might include `.this`, if it contains a class),
      *  returns `pre.C.foo`.
      *
      *  If `sym` refers to a local definition `foo`, returns
      *  a tree representing `foo`.
      *
      *  @note In all cases, the constructed tree should only
      *  be spliced into the places where such accesses make sense.
      *  For example, it is incorrect to have `C.this.foo` outside
      *  the class body of `C`, or have `foo` outside the lexical
      *  scope for the definition of `foo`.
      */
    def apply(sym: Symbol): Ref

    /** Creates a reference tree from a symbol */
    def make(sym: Symbol): Ref
  }

}

/////// Ident ///////////////////////////////////////////////////////////////

/** Tree representing a reference to definition with a given name. */
trait Ident private[compiletime] () extends Ref {

  /** Name of this `Ident`. */
  def name: String
}
object Ident {

  def quoted(using quotes: Quotes): Ident.Module = quotes.reflectV2.Ident
  given moduleConversion: (quotes: Quotes) => Conversion[Ident.type, Ident.Module] = _ => quotes.reflectV2.Ident

  /** Matches a term identifier and returns its name. */
  def unapply(tree: Ident): Some[String] = Some(tree.name)

  trait Module private[compiletime] () {
    def apply(tmref: TermRef): Term
    def make(tmref: TermRef): Term
  }

}

/////// Wildcard ///////////////////////////////////////////////////////////////

/** Pattern representing a `_` wildcard. */
trait Wildcard private[compiletime] () extends Ident
object Wildcard {

  def quoted(using quotes: Quotes): Wildcard.Module = quotes.reflectV2.Wildcard
  given moduleConversion: (quotes: Quotes) => Conversion[Wildcard.type, Wildcard.Module] = _ => quotes.reflectV2.Wildcard

  def unapply(wildcard: Wildcard): true = true

  trait Module private[compiletime] () {

    /** Creates a tree representing a `_` wildcard. */
    def apply(): Wildcard

    /** Creates a tree representing a `_` wildcard. */
    def make(): Wildcard
  }

}

/////// Select ///////////////////////////////////////////////////////////////

/** Tree representing a selection of definition with a given name on a given prefix. */
trait Select private[compiletime] () extends Ref {

  /** Qualifier of the `qualifier.name`. */
  def qualifier: Term

  /** Name of this `Select`. */
  def name: String

  /** Signature of this method. */
  def signature: Option[Signature]
}
object Select {

  def quoted(using quotes: Quotes): Select.Module = quotes.reflectV2.Select
  given moduleConversion: (quotes: Quotes) => Conversion[Select.type, Select.Module] = _ => quotes.reflectV2.Select

  def unapply(x: Select): (Term, String) = (x.qualifier, x.name)

  trait Module private[compiletime] () {

    /** Selects a term member by symbol. */
    def apply(qualifier: Term, symbol: Symbol): Select

    /** Selects a term member by symbol. */
    def make(qualifier: Term, symbol: Symbol): Select

    /**
      * Selects a field or a non-overloaded method by name
      *
      *  @note The method will produce an assertion error if the selected
      *        method is overloaded. The method `overloaded` should be used
      *        in that case.
      */
    def unique(qualifier: Term, name: String): Select

    /** Calls an overloaded method with the given type and term parameters. */
    def overloaded(qualifier: Term, name: String, targs: List[TypeRepr], args: List[Term]): Term

    /** Calls an overloaded method with the given type and term parameters. */
    def overloaded(qualifier: Term, name: String, targs: List[TypeRepr], args: List[Term], returnType: TypeRepr): Term
  }

}

/////// Literal ///////////////////////////////////////////////////////////////

/** Tree representing a literal value in the source code. */
trait Literal private[compiletime] () extends Term {

  /** Value of this literal. */
  def constant: Constant
}
object Literal {

  def quoted(using quotes: Quotes): Literal.Module = quotes.reflectV2.Literal
  given moduleConversion: (quotes: Quotes) => Conversion[Literal.type, Literal.Module] = _ => quotes.reflectV2.Literal

  def unapply(x: Literal): Some[Constant] = Some(x.constant)

  trait Module private[compiletime] () {

    /** Creates a literal constant. */
    def apply(constant: Constant): Literal

    /** Creates a literal constant. */
    def make(constant: Constant): Literal
  }

}

/////// This ///////////////////////////////////////////////////////////////

/** Tree representing `this` or `C.this` in the source code. */
trait This private[compiletime] () extends Term {

  /**
    * Returns `C` if the underlying tree is of the form `C.this`
    *
    *  Otherwise, return `None`.
    */
  def id: Option[String]
}
object This {

  def quoted(using quotes: Quotes): This.Module = quotes.reflectV2.This
  given moduleConversion: (quotes: Quotes) => Conversion[This.type, This.Module] = _ => quotes.reflectV2.This

  def unapply(x: This): Some[Option[String]] = Some(x.id)

  trait Module private[compiletime] () {

    /** Creates a `C.this` for `C` pointing to `cls`. */
    def apply(cls: Symbol): This

    /** Creates a `C.this` for `C` pointing to `cls`. */
    def make(cls: Symbol): This
  }

}

/////// New ///////////////////////////////////////////////////////////////

/** Tree representing `new` in the source code. */
trait New private[compiletime] () extends Term {

  /** Returns the type tree of this `new`. */
  def tpt: TypeTree
}
object New {

  def quoted(using quotes: Quotes): New.Module = quotes.reflectV2.New
  given moduleConversion: (quotes: Quotes) => Conversion[New.type, New.Module] = _ => quotes.reflectV2.New

  def unapply(x: New): Some[TypeTree] = Some(x.tpt)

  trait Module private[compiletime] () {

    /** Creates a `new <tpt: TypeTree>`. */
    def apply(tpt: TypeTree): New

    /** Creates a `new <tpt: TypeTree>`. */
    def make(tpt: TypeTree): New
  }

}

/////// NamedArg ///////////////////////////////////////////////////////////////

/** Tree representing an argument passed with an explicit name. Such as `arg1 = x` in `foo(arg1 = x)`. */
trait NamedArg private[compiletime] () extends Term {

  /** The name part of `name = arg`. */
  def name: String

  /** The argument part of `name = arg`. */
  def value: Term
}
object NamedArg {

  def quoted(using quotes: Quotes): NamedArg.Module = quotes.reflectV2.NamedArg
  given moduleConversion: (quotes: Quotes) => Conversion[NamedArg.type, NamedArg.Module] = _ => quotes.reflectV2.NamedArg

  def unapply(x: NamedArg): (String, Term) = (x.name, x.value)

  trait Module private[compiletime] () {

    /** Creates a named argument `<name: String> = <value: Term>`. */
    def apply(name: String, arg: Term): NamedArg

    /** Creates a named argument `<name: String> = <value: Term>`. */
    def make(name: String, arg: Term): NamedArg
  }

}

/////// Apply ///////////////////////////////////////////////////////////////

/**
  * Tree representing an application of arguments.
  *  It represents a single list of arguments, multiple argument lists will have nested `Apply`s
  */
trait Apply private[compiletime] () extends Term {

  /**
    * The `fun` part of an (implicit) application like `fun(args)`
    *
    *  It may be a partially applied method:
    *  ```scala
    *  def f(x1: Int)(x2: Int) = ???
    *  f(1)(2)
    *  ```
    *  - `fun` is `f(1)` in the `Apply` of `f(1)(2)`
    *  - `fun` is `f` in the `Apply` of `f(1)`
    */
  def fun: Term

  /**
    * The arguments (implicitly) passed to the method
    *
    *  The `Apply` may be a partially applied method:
    *  ```scala
    *  def f(x1: Int)(x2: Int) = ???
    *  f(1)(2)
    *  ```
    *  - `args` is `(2)` in the `Apply` of `f(1)(2)`
    *  - `args` is `(1)` in the `Apply` of `f(1)`
    */
  def args: List[Term]
}
object Apply {

  def quoted(using quotes: Quotes): Apply.Module = quotes.reflectV2.Apply
  given moduleConversion: (quotes: Quotes) => Conversion[Apply.type, Apply.Module] = _ => quotes.reflectV2.Apply

  def unapply(x: Apply): (Term, List[Term]) = (x.fun, x.args)

  trait Module private[compiletime] () {

    /** Creates a function application `<fun: Term>(<args: List[Term]>)`. */
    def apply(fun: Term, args: List[Term]): Apply

    /** Creates a function application `<fun: Term>(<args: List[Term]>)`. */
    def make(fun: Term, args: List[Term]): Apply
  }

}

/////// TypeApply ///////////////////////////////////////////////////////////////

/** Tree representing an application of type arguments. */
trait TypeApply private[compiletime] () extends Term {

  /**
    * The `fun` part of an (inferred) type application like `fun[Args]`
    *
    *  It may be a partially applied method:
    *  ```scala
    *  //{
    *  type T
    *  //}
    *  extension (x: Int) def f[T](y: T) = ???
    *  // represented as
    *  // def f(x: Int)[T](y: T) = ???
    *
    *  1.f[Int](2)
    *  // represented as
    *  // f(1)[Int](2)
    *  ```
    *  - `fun` is `f(1)` in the `TypeApply` of `f(1)[Int]`
    */
  def fun: Term

  /**
    * The (inferred) type arguments passed to the method
    *
    *  The `TypeApply` may be a partially applied method:
    *  ```scala
    *  //{
    *  type T
    *  //}
    *  extension (x: Int) def f[T](y: T) = ???
    *  // represented as
    *  // def f(x: Int)[T](y: T) = ???
    *
    *  1.f[Int](2)
    *  // represented as
    *  // f(1)[Int](2)
    *  ```
    *  - `fun` is `[Int]` in the `TypeApply` of `f(1)[Int]`
    */
  def args: List[TypeTree]
}
object TypeApply {

  def quoted(using quotes: Quotes): TypeApply.Module = quotes.reflectV2.TypeApply
  given moduleConversion: (quotes: Quotes) => Conversion[TypeApply.type, TypeApply.Module] = _ => quotes.reflectV2.TypeApply

  def unapply(x: TypeApply): (Term, List[TypeTree]) = (x.fun, x.args)

  trait Module private[compiletime] () {

    /** Creates a function type application `<fun: Term>[<args: List[TypeTree]>]`. */
    def apply(fun: Term, args: List[TypeTree]): TypeApply

    /** Creates a function type application `<fun: Term>[<args: List[TypeTree]>]`. */
    def make(fun: Term, args: List[TypeTree]): TypeApply
  }

}

/////// Super ///////////////////////////////////////////////////////////////

/** Tree representing `super` in the source code. */
trait Super private[compiletime] () extends Term {
  def qualifier: Term
  def id: Option[String]
  def idPos: Position
}
object Super {

  def quoted(using quotes: Quotes): Super.Module = quotes.reflectV2.Super
  given moduleConversion: (quotes: Quotes) => Conversion[Super.type, Super.Module] = _ => quotes.reflectV2.Super

  def unapply(x: Super): (Term, Option[String]) = (x.qualifier, x.id)

  trait Module private[compiletime] () {

    /** Creates a `<qualifier: Term>.super[<id: Option[Id]>`. */
    def apply(qual: Term, mix: Option[String]): Super

    /** Creates a `<qualifier: Term>.super[<id: Option[Id]>`. */
    def make(qual: Term, mix: Option[String]): Super
  }

}

/////// Typed ///////////////////////////////////////////////////////////////

/**
  * Tree representing a type ascription `x: T` in the source code.
  *
  *  Also represents a pattern that contains a term `x`.
  *  Other `: T` patterns use the more general `TypedOrTest`.
  */
trait Typed private[compiletime] () extends Term, TypedOrTest {
  def expr: Term
  def tpt: TypeTree
}
object Typed {

  def quoted(using quotes: Quotes): Typed.Module = quotes.reflectV2.Typed
  given moduleConversion: (quotes: Quotes) => Conversion[Typed.type, Typed.Module] = _ => quotes.reflectV2.Typed

  def unapply(x: Typed): (Term, TypeTree) = (x.expr, x.tpt)

  trait Module private[compiletime] () {

    /** Creates a type ascription `<x: Term>: <tpt: TypeTree>`. */
    def apply(expr: Term, tpt: TypeTree): Typed

    /** Creates a type ascription `<x: Term>: <tpt: TypeTree>`. */
    def make(expr: Term, tpt: TypeTree): Typed
  }

}

/////// Assign ///////////////////////////////////////////////////////////////

/** Tree representing an assignment `x = y` in the source code. */
trait Assign private[compiletime] () extends Term {
  def lhs: Term
  def rhs: Term
}
object Assign {

  def quoted(using quotes: Quotes): Assign.Module = quotes.reflectV2.Assign
  given moduleConversion: (quotes: Quotes) => Conversion[Assign.type, Assign.Module] = _ => quotes.reflectV2.Assign

  def unapply(x: Assign): (Term, Term) = (x.lhs, x.rhs)

  trait Module private[compiletime] () {

    /** Creates an assignment `<lhs: Term> = <rhs: Term>`. */
    def apply(lhs: Term, rhs: Term): Assign

    /** Creates an assignment `<lhs: Term> = <rhs: Term>`. */
    def make(lhs: Term, rhs: Term): Assign
  }

}

/////// Block ///////////////////////////////////////////////////////////////

/** Tree representing a block `{ ... }` in the source code. */
trait Block private[compiletime] () extends Term {
  def statements: List[Statement]
  def expr: Term
}
object Block {

  def quoted(using quotes: Quotes): Block.Module = quotes.reflectV2.Block
  given moduleConversion: (quotes: Quotes) => Conversion[Block.type, Block.Module] = _ => quotes.reflectV2.Block

  def unapply(x: Block): (List[Statement], Term) = (x.statements, x.expr)

  trait Module private[compiletime] () {

    /** Creates a block `{ <statements: List[Statement]>; <expr: Term> }`. */
    def apply(stats: List[Statement], expr: Term): Block

    /** Creates a block `{ <statements: List[Statement]>; <expr: Term> }`. */
    def make(stats: List[Statement], expr: Term): Block
  }

}

/////// Closure ///////////////////////////////////////////////////////////////

/**
  * A lambda `(...) => ...` in the source code is represented as
  *  a local method and a closure:
  *
  *  {
  *    def m(...) = ...
  *    closure(m)
  *  }
  */
trait Closure private[compiletime] () extends Term {
  def meth: Term
  def tpeOpt: Option[TypeRepr]
}
object Closure {

  def quoted(using quotes: Quotes): Closure.Module = quotes.reflectV2.Closure
  given moduleConversion: (quotes: Quotes) => Conversion[Closure.type, Closure.Module] = _ => quotes.reflectV2.Closure

  def unapply(x: Closure): (Term, Option[TypeRepr]) = (x.meth, x.tpeOpt)

  trait Module private[compiletime] () {
    def apply(meth: Term, tpe: Option[TypeRepr]): Closure
    def make(meth: Term, tpe: Option[TypeRepr]): Closure
  }

}

/////// Lambda ///////////////////////////////////////////////////////////////

/**
  * A lambda `(...) => ...` in the source code is represented as
  *  a local method and a closure:
  *
  *  ```scala sc:nocompile
  *  {
  *    def m(...) = ...
  *    closure(m)
  *  }
  *  ```
  *  @note Due to the encoding, in pattern matches the case for `Lambda`
  *        should come before the case for `Block` to avoid mishandling
  *        of `Lambda`.
  */
object Lambda {

  def quoted(using quotes: Quotes): Lambda.Module = quotes.reflectV2.Lambda
  given moduleConversion: (quotes: Quotes) => Conversion[Lambda.type, Lambda.Module] = _ => quotes.reflectV2.Lambda

  trait Module private[compiletime] () {

    /**
      * Matches a lambda definition of the form
      *  ```scala sc:nocompile
      *  Block((DefDef(_, _, params :: Nil, _, Some(body))) :: Nil, Closure(meth, _))
      *  ```
      *  Extracts the parameter definitions and body.
      */
    def unapply(tree: Block): Option[(List[ValDef], Term)]

    /**
      * Generates a lambda with the given method type.
      *  ```scala sc:nocompile
      *  Block((DefDef(_, _, params :: Nil, _, Some(rhsFn(meth, paramRefs)))) :: Nil, Closure(meth, _))
      *  ```
      *
      * Usage:
      *  ```
      *  val mtpe = MethodType(List("arg1"))(_ => List(TypeRepr.of[Int]), _ => TypeRepr.of[Int])
      *  Lambda(owner, mtpe, {
      *    case (methSym, List(arg1: Term)) =>
      *      ValDef.let(methSym, f(arg1)) { ... }
      *    }
      *  )
      *  ```
      *
      *  Usage with quotes:
      *  ```
      *  val mtpe = MethodType(List("arg1"))(_ => List(TypeRepr.of[Int]), _ => TypeRepr.of[Int])
      *  Lambda(owner, mtpe, {
      *    case (methSym, List(arg1: Term)) =>
      *      given Quotes = methSym.asQuotes
      *      '{ ... }
      *    }
      *  )
      *  ```
      *
      *  @param owner owner of the generated `meth` symbol
      *  @param tpe Type of the definition
      *  @param rhsFn Function that receives the `meth` symbol and the a list of references to the `params`
      */
    def apply(owner: Symbol, tpe: MethodType, rhsFn: (Symbol, List[Tree]) => Tree): Block

    /** Generates a lambda with the given method type. */
    def make(owner: Symbol, tpe: MethodType, rhsFn: (Symbol, List[Tree]) => Tree): Block
  }

}

/////// If ///////////////////////////////////////////////////////////////

/** Tree representing an if/then/else `if (...) ... else ...` in the source code. */
trait If private[compiletime] () extends Term {
  def cond: Term
  def thenp: Term
  def elsep: Term
  def isInline: Boolean
}
object If {

  def quoted(using quotes: Quotes): If.Module = quotes.reflectV2.If
  given moduleConversion: (quotes: Quotes) => Conversion[If.type, If.Module] = _ => quotes.reflectV2.If

  def unapply(tree: If): (Term, Term, Term) = (tree.cond, tree.thenp, tree.elsep)

  trait Module private[compiletime] () {

    /** Creates an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>`. */
    def apply(cond: Term, thenp: Term, elsep: Term): If

    /** Creates an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>`. */
    def make(cond: Term, thenp: Term, elsep: Term): If
  }

}

/////// Match ///////////////////////////////////////////////////////////////

/** Tree representing a pattern match `x match  { ... }` in the source code. */
trait Match private[compiletime] () extends Term {
  def scrutinee: Term
  def cases: List[CaseDef]
  def isInline: Boolean
}
object Match {

  def quoted(using quotes: Quotes): Match.Module = quotes.reflectV2.Match
  given moduleConversion: (quotes: Quotes) => Conversion[Match.type, Match.Module] = _ => quotes.reflectV2.Match

  def unapply(x: Match): (Term, List[CaseDef]) = (x.scrutinee, x.cases)

  trait Module private[compiletime] () {

    /** Creates a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }`. */
    def apply(selector: Term, cases: List[CaseDef]): Match

    /** Creates a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }`. */
    def make(selector: Term, cases: List[CaseDef]): Match
  }

}

/////// SummonFrom ///////////////////////////////////////////////////////////////

/** Tree representing a summoning match `summonFrom { ... }` in the source code. */
trait SummonFrom private[compiletime] () extends Term {
  def cases: List[CaseDef]
}
object SummonFrom {

  def quoted(using quotes: Quotes): SummonFrom.Module = quotes.reflectV2.SummonFrom
  given moduleConversion: (quotes: Quotes) => Conversion[SummonFrom.type, SummonFrom.Module] = _ => quotes.reflectV2.SummonFrom

  def unapply(x: SummonFrom): Some[List[CaseDef]] = Some(x.cases)

  trait Module private[compiletime] () {

    /** Creates a pattern match `given match { <cases: List[CaseDef]> }`. */
    def apply(cases: List[CaseDef]): SummonFrom

    /** Creates a pattern match `given match { <cases: List[CaseDef]> }`. */
    def make(cases: List[CaseDef]): SummonFrom
  }

}

/////// Try ///////////////////////////////////////////////////////////////

/** Tree representing a try catch `try x catch { ... } finally { ... }` in the source code. */
trait Try private[compiletime] () extends Term {
  def body: Term
  def cases: List[CaseDef]
  def finalizer: Option[Term]
}
object Try {

  def quoted(using quotes: Quotes): Try.Module = quotes.reflectV2.Try
  given moduleConversion: (quotes: Quotes) => Conversion[Try.type, Try.Module] = _ => quotes.reflectV2.Try

  def unapply(x: Try): (Term, List[CaseDef], Option[Term]) = (x.body, x.cases, x.finalizer)

  trait Module private[compiletime] () {

    /** Creates a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>`. */
    def apply(expr: Term, cases: List[CaseDef], finalizer: Option[Term]): Try

    /** Creates a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>`. */
    def make(expr: Term, cases: List[CaseDef], finalizer: Option[Term]): Try
  }

}

/////// Return ///////////////////////////////////////////////////////////////

/** Tree representing a `return` in the source code. */
trait Return private[compiletime] () extends Term {
  def expr: Term
  def from: Symbol
}
object Return {

  def quoted(using quotes: Quotes): Return.Module = quotes.reflectV2.Return
  given moduleConversion: (quotes: Quotes) => Conversion[Return.type, Return.Module] = _ => quotes.reflectV2.Return

  def unapply(x: Return): (Term, Symbol) = (x.expr, x.from)

  trait Module private[compiletime] () {

    /** Creates `return <expr: Term>`. */
    def apply(expr: Term, from: Symbol): Return

    /** Creates `return <expr: Term>`. */
    def make(expr: Term, from: Symbol): Return
  }

}

/////// Repeated ///////////////////////////////////////////////////////////////

/**
  * Tree representing a variable argument list in the source code.
  *
  *  This tree is used to encode varargs terms. The Repeated encapsulates
  *  the sequence of the elements but needs to be wrapped in a
  *  `scala.<repeated>[T]` (see `defn.RepeatedParamClass`). For example the
  *   arguments `1, 2` of `List.apply(1, 2)` can be represented as follows:
  *
  *  ```scala
  *  //{
  *  import scala.quoted._
  *  def inQuotes(using Quotes) = {
  *    val q: Quotes = summon[Quotes]
  *    import q.reflect._
  *  //}
  *    val intArgs = List(Literal(IntConstant(1)), Literal(IntConstant(2)))
  *    Typed(
  *      Repeated(intArgs, TypeTree.of[Int]),
  *      Inferred(defn.RepeatedParamClass.typeRef.appliedTo(TypeRepr.of[Int]))
  *    )
  *  //{
  *  }
  *  //}
  *  ```
  */
trait Repeated private[compiletime] () extends Term {
  def elems: List[Term]
  def elemtpt: TypeTree
}
object Repeated {

  def quoted(using quotes: Quotes): Repeated.Module = quotes.reflectV2.Repeated
  given moduleConversion: (quotes: Quotes) => Conversion[Repeated.type, Repeated.Module] = _ => quotes.reflectV2.Repeated

  def unapply(x: Repeated): (List[Term], TypeTree) = (x.elems, x.elemtpt)

  trait Module private[compiletime] () {

    /** Creates a literal sequence of elements. */
    def apply(elems: List[Term], tpt: TypeTree): Repeated

    /** Creates a literal sequence of elements. */
    def make(elems: List[Term], tpt: TypeTree): Repeated
  }

}

/////// Inlined ///////////////////////////////////////////////////////////////

/** Tree representing the scope of an inlined tree. */
trait Inlined private[compiletime] () extends Term {
  def call: Option[Tree]
  def bindings: List[Definition]
  def body: Term
}
object Inlined {

  def quoted(using quotes: Quotes): Inlined.Module = quotes.reflectV2.Inlined
  given moduleConversion: (quotes: Quotes) => Conversion[Inlined.type, Inlined.Module] = _ => quotes.reflectV2.Inlined

  def unapply(x: Inlined): (Option[Tree], List[Definition], Term) = (x.call, x.bindings, x.body)

  trait Module private[compiletime] () {
    def apply(call: Option[Tree], bindings: List[Definition], expansion: Term): Inlined
    def make(call: Option[Tree], bindings: List[Definition], expansion: Term): Inlined
  }

}

/////// SelectOuter ///////////////////////////////////////////////////////////////

/** Tree representing a selection of definition with a given name on a given prefix and number of nested scopes of inlined trees. */
trait SelectOuter private[compiletime] () extends Term {
  def qualifier: Term
  def name: String
  def level: Int
}
object SelectOuter {

  def quoted(using quotes: Quotes): SelectOuter.Module = quotes.reflectV2.SelectOuter
  given moduleConversion: (quotes: Quotes) => Conversion[SelectOuter.type, SelectOuter.Module] = _ => quotes.reflectV2.SelectOuter

  def unapply(x: SelectOuter): (Term, String, Int) = (x.qualifier, x.name, x.level)

  trait Module private[compiletime] () {
    def apply(qualifier: Term, name: String, levels: Int): SelectOuter
    def make(qualifier: Term, name: String, levels: Int): SelectOuter
  }

}

/////// While ///////////////////////////////////////////////////////////////

/** Tree representing a while loop. */
trait While private[compiletime] () extends Term {
  def cond: Term
  def body: Term
}
object While {

  def quoted(using quotes: Quotes): While.Module = quotes.reflectV2.While
  given moduleConversion: (quotes: Quotes) => Conversion[While.type, While.Module] = _ => quotes.reflectV2.While

  def unapply(x: While): (Term, Term) = (x.cond, x.body)

  trait Module private[compiletime] () {

    /** Creates a while loop `while (<cond>) <body>` and returns (<cond>, <body>). */
    def apply(cond: Term, body: Term): While

    /** Creates a while loop `while (<cond>) <body>` and returns (<cond>, <body>). */
    def make(cond: Term, body: Term): While
  }

}

/////// TypedOrTest ///////////////////////////////////////////////////////////////

/** Tree representing a type ascription or type test pattern `x: T` in the source code. */
sealed trait TypedOrTest private[compiletime] () extends Tree {
  def tree: Tree
  def tpt: TypeTree
}
object TypedOrTest {

  def quoted(using quotes: Quotes): TypedOrTest.Module = quotes.reflectV2.TypedOrTest
  given moduleConversion: (quotes: Quotes) => Conversion[TypedOrTest.type, TypedOrTest.Module] = _ => quotes.reflectV2.TypedOrTest

  def unapply(x: TypedOrTest): (Tree, TypeTree) = (x.tree, x.tpt)

  trait Module private[compiletime] () {

    /** Creates a type ascription `<x: Tree>: <tpt: TypeTree>`. */
    def apply(expr: Tree, tpt: TypeTree): TypedOrTest

    /** Creates a type ascription `<x: Tree>: <tpt: TypeTree>`. */
    def make(expr: Tree, tpt: TypeTree): TypedOrTest
  }

}

/////// TypeTree ///////////////////////////////////////////////////////////////

/** Type tree representing a type written in the source. */
sealed trait TypeTree private[compiletime] () extends Tree {

  /** TypeRepr of this type tree. */
  def tpe: TypeRepr
}
object TypeTree {

  def quoted(using quotes: Quotes): TypeTree.Module = quotes.reflectV2.TypeTree
  given moduleConversion: (quotes: Quotes) => Conversion[TypeTree.type, TypeTree.Module] = _ => quotes.reflectV2.TypeTree

  trait Module private[compiletime] () {

    /** Returns the tree of type or kind (TypeTree) of T. */
    def of[T <: AnyKind](using Type[T]): TypeTree

    /**
      * Returns a type tree reference to the symbol
      *
      *  @param typeSymbol The type symbol for which we are creating a type tree reference.
      */
    def ref(typeSymbol: Symbol): TypeTree
  }

}

/////// Inferred ///////////////////////////////////////////////////////////////

/** Type tree representing an inferred type. */
trait Inferred private[compiletime] () extends TypeTree
object Inferred {

  def quoted(using quotes: Quotes): Inferred.Module = quotes.reflectV2.Inferred
  given moduleConversion: (quotes: Quotes) => Conversion[Inferred.type, Inferred.Module] = _ => quotes.reflectV2.Inferred

  def unapply(x: Inferred): true = true

  trait Module private[compiletime] () {
    def apply(tpe: TypeRepr): Inferred
    def make(tpe: TypeRepr): Inferred
  }

}

/////// TypeIdent ///////////////////////////////////////////////////////////////

/** Type tree representing a reference to definition with a given name. */
trait TypeIdent private[compiletime] () extends TypeTree {
  def name: String
}
object TypeIdent {

  def quoted(using quotes: Quotes): TypeIdent.Module = quotes.reflectV2.TypeIdent
  given moduleConversion: (quotes: Quotes) => Conversion[TypeIdent.type, TypeIdent.Module] = _ => quotes.reflectV2.TypeIdent

  def unapply(x: TypeIdent): Some[String] = Some(x.name)

  trait Module private[compiletime] () {
    def apply(sym: Symbol): TypeTree
    def make(sym: Symbol): TypeTree
  }

}

/////// TypeSelect ///////////////////////////////////////////////////////////////

/** Type tree representing a selection of definition with a given name on a given term prefix. */
trait TypeSelect private[compiletime] () extends TypeTree {
  def qualifier: Term
  def name: String
}
object TypeSelect {

  def quoted(using quotes: Quotes): TypeSelect.Module = quotes.reflectV2.TypeSelect
  given moduleConversion: (quotes: Quotes) => Conversion[TypeSelect.type, TypeSelect.Module] = _ => quotes.reflectV2.TypeSelect

  def unapply(x: TypeSelect): (Term, String) = (x.qualifier, x.name)

  trait Module private[compiletime] () {
    def apply(qualifier: Term, name: String): TypeSelect
    def make(qualifier: Term, name: String): TypeSelect
  }

}

/////// TypeProjection ///////////////////////////////////////////////////////////////

/** Type tree representing a selection of definition with a given name on a given type prefix. */
trait TypeProjection private[compiletime] () extends TypeTree {
  def qualifier: TypeTree
  def name: String
}
object TypeProjection {

  def quoted(using quotes: Quotes): TypeProjection.Module = quotes.reflectV2.TypeProjection
  given moduleConversion: (quotes: Quotes) => Conversion[TypeProjection.type, TypeProjection.Module] = _ => quotes.reflectV2.TypeProjection

  def unapply(x: TypeProjection): (TypeTree, String) = (x.qualifier, x.name)

  trait Module private[compiletime] () {
    def apply(qualifier: TypeTree, name: String): TypeProjection
    def make(qualifier: TypeTree, name: String): TypeProjection
  }

}

/////// Singleton ///////////////////////////////////////////////////////////////

/** Type tree representing a singleton type. */
trait Singleton private[compiletime] () extends TypeTree {
  def ref: Term
}
object Singleton {

  def quoted(using quotes: Quotes): Singleton.Module = quotes.reflectV2.Singleton
  given moduleConversion: (quotes: Quotes) => Conversion[Singleton.type, Singleton.Module] = _ => quotes.reflectV2.Singleton

  def unapply(x: Singleton): Some[Term] = Some(x.ref)

  trait Module private[compiletime] () {
    def apply(ref: Term): Singleton
    def make(ref: Term): Singleton
  }

}

/////// Refined ///////////////////////////////////////////////////////////////

/** Type tree representing a type refinement. */
trait Refined private[compiletime] () extends TypeTree {
  def tpt: TypeTree
  def refinements: List[Definition]
}
object Refined {

  def quoted(using quotes: Quotes): Refined.Module = quotes.reflectV2.Refined
  given moduleConversion: (quotes: Quotes) => Conversion[Refined.type, Refined.Module] = _ => quotes.reflectV2.Refined

  def unapply(x: Refined): (TypeTree, List[Definition]) = (x.tpt, x.refinements)

  trait Module private[compiletime] () {

    /**
      * Creates and types a Refined AST node.
      * @param tpt - parent type being refined
      * @param refinements - List of definitions represesenting refinements
      * @param refineCls - symbol of the class of which the refinement definitions originally come from
      * @return
      */
    def apply(tpt: TypeTree, refinements: List[Definition], refineCls: Symbol): Refined

    /** Creates and types a Refined AST node. */
    def make(tpt: TypeTree, refinements: List[Definition], refineCls: Symbol): Refined
  }

}

/////// Applied ///////////////////////////////////////////////////////////////

/** Type tree representing a type application. */
trait Applied private[compiletime] () extends TypeTree {
  def tpt: TypeTree
  def args: List[Tree]
}
object Applied {

  def quoted(using quotes: Quotes): Applied.Module = quotes.reflectV2.Applied
  given moduleConversion: (quotes: Quotes) => Conversion[Applied.type, Applied.Module] = _ => quotes.reflectV2.Applied

  def unapply(x: Applied): (TypeTree, List[Tree]) = (x.tpt, x.args)

  trait Module private[compiletime] () {
    def apply(tpt: TypeTree, args: List[Tree]): Applied
    def make(tpt: TypeTree, args: List[Tree]): Applied
  }

}

/////// Annotated ///////////////////////////////////////////////////////////////

/** Type tree representing an annotated type. */
trait Annotated private[compiletime] () extends TypeTree {
  def arg: TypeTree
  def annotation: Term
}
object Annotated {

  def quoted(using quotes: Quotes): Annotated.Module = quotes.reflectV2.Annotated
  given moduleConversion: (quotes: Quotes) => Conversion[Annotated.type, Annotated.Module] = _ => quotes.reflectV2.Annotated

  def unapply(x: Annotated): (TypeTree, Term) = (x.arg, x.annotation)

  trait Module private[compiletime] () {
    def apply(arg: TypeTree, annotation: Term): Annotated
    def make(arg: TypeTree, annotation: Term): Annotated
  }

}

/////// MatchTypeTree ///////////////////////////////////////////////////////////////

/** Type tree representing a type match. */
trait MatchTypeTree private[compiletime] () extends TypeTree {
  def bound: Option[TypeTree]
  def selector: TypeTree
  def cases: List[TypeCaseDef]
}
object MatchTypeTree {

  def quoted(using quotes: Quotes): MatchTypeTree.Module = quotes.reflectV2.MatchTypeTree
  given moduleConversion: (quotes: Quotes) => Conversion[MatchTypeTree.type, MatchTypeTree.Module] = _ => quotes.reflectV2.MatchTypeTree

  def unapply(x: MatchTypeTree): (Option[TypeTree], TypeTree, List[TypeCaseDef]) = (x.bound, x.selector, x.cases)

  trait Module private[compiletime] () {
    def apply(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef]): MatchTypeTree
    def make(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef]): MatchTypeTree
  }

}

/////// ByName ///////////////////////////////////////////////////////////////

/** Type tree representing a by name parameter. */
trait ByName private[compiletime] () extends TypeTree {
  def result: TypeTree
}
object ByName {

  def quoted(using quotes: Quotes): ByName.Module = quotes.reflectV2.ByName
  given moduleConversion: (quotes: Quotes) => Conversion[ByName.type, ByName.Module] = _ => quotes.reflectV2.ByName

  def unapply(x: ByName): Some[TypeTree] = Some(x.result)

  trait Module private[compiletime] () {
    def apply(result: TypeTree): ByName
    def make(result: TypeTree): ByName
  }

}

/////// LambdaTypeTree ///////////////////////////////////////////////////////////////

/** Type tree representing a lambda abstraction type. */
trait LambdaTypeTree private[compiletime] () extends TypeTree {
  def tparams: List[TypeDef]
  def body: Tree
}
object LambdaTypeTree {

  def quoted(using quotes: Quotes): LambdaTypeTree.Module = quotes.reflectV2.LambdaTypeTree
  given moduleConversion: (quotes: Quotes) => Conversion[LambdaTypeTree.type, LambdaTypeTree.Module] = _ => quotes.reflectV2.LambdaTypeTree

  def unapply(tree: LambdaTypeTree): (List[TypeDef], Tree) = (tree.tparams, tree.body)

  trait Module private[compiletime] () {
    def apply(tparams: List[TypeDef], body: Tree): LambdaTypeTree
    def make(tparams: List[TypeDef], body: Tree): LambdaTypeTree
  }

}

/////// TypeBind ///////////////////////////////////////////////////////////////

/** Type tree representing a type binding. */
trait TypeBind private[compiletime] () extends TypeTree {
  def name: String
  def body: Tree
}
object TypeBind {

  def quoted(using quotes: Quotes): TypeBind.Module = quotes.reflectV2.TypeBind
  given moduleConversion: (quotes: Quotes) => Conversion[TypeBind.type, TypeBind.Module] = _ => quotes.reflectV2.TypeBind

  def unapply(x: TypeBind): (String, Tree) = (x.name, x.body)

  trait Module private[compiletime] () {}

}

/////// TypeBlock ///////////////////////////////////////////////////////////////

/** Type tree within a block with aliases `{ type U1 = ... ; T[U1, U2] }`. */
trait TypeBlock private[compiletime] () extends TypeTree {
  def aliases: List[TypeDef]
  def tpt: TypeTree
}
object TypeBlock {

  def quoted(using quotes: Quotes): TypeBlock.Module = quotes.reflectV2.TypeBlock
  given moduleConversion: (quotes: Quotes) => Conversion[TypeBlock.type, TypeBlock.Module] = _ => quotes.reflectV2.TypeBlock

  def unapply(x: TypeBlock): (List[TypeDef], TypeTree) = (x.aliases, x.tpt)

  trait Module private[compiletime] () {
    def apply(aliases: List[TypeDef], tpt: TypeTree): TypeBlock
    def make(aliases: List[TypeDef], tpt: TypeTree): TypeBlock
  }

}

/////// TypeBoundsTree ///////////////////////////////////////////////////////////////

/** Type tree representing a type bound written in the source. */
trait TypeBoundsTree private[compiletime] () extends Tree {
  def tpe: TypeBounds
  def low: TypeTree
  def hi: TypeTree
}
object TypeBoundsTree {

  def quoted(using quotes: Quotes): TypeBoundsTree.Module = quotes.reflectV2.TypeBoundsTree
  given moduleConversion: (quotes: Quotes) => Conversion[TypeBoundsTree.type, TypeBoundsTree.Module] = _ => quotes.reflectV2.TypeBoundsTree

  def unapply(x: TypeBoundsTree): (TypeTree, TypeTree) = (x.low, x.hi)

  trait Module private[compiletime] () {
    def apply(low: TypeTree, hi: TypeTree): TypeBoundsTree
    def make(low: TypeTree, hi: TypeTree): TypeBoundsTree
  }

}

/////// WildcardTypeTree ///////////////////////////////////////////////////////////////

/**
  * Type tree representing wildcard type bounds written in the source.
  *  The wildcard type `?` (for example in in `List[?]`) will be a type tree that
  *  represents a type but has `TypeBounds` inside.
  */
trait WildcardTypeTree private[compiletime] () extends Tree {
  def tpe: TypeRepr
}
object WildcardTypeTree {

  def quoted(using quotes: Quotes): WildcardTypeTree.Module = quotes.reflectV2.WildcardTypeTree
  given moduleConversion: (quotes: Quotes) => Conversion[WildcardTypeTree.type, WildcardTypeTree.Module] = _ => quotes.reflectV2.WildcardTypeTree

  def unapply(x: WildcardTypeTree): true = true

  trait Module private[compiletime] () {
    def apply(tpe: TypeRepr): WildcardTypeTree
    def make(tpe: TypeRepr): WildcardTypeTree
  }

}

/////// CaseDef ///////////////////////////////////////////////////////////////

/** Branch of a pattern match or catch clause. */
trait CaseDef private[compiletime] () extends Tree {
  def pattern: Tree
  def guard: Option[Term]
  def rhs: Term
}
object CaseDef {

  def quoted(using quotes: Quotes): CaseDef.Module = quotes.reflectV2.CaseDef
  given moduleConversion: (quotes: Quotes) => Conversion[CaseDef.type, CaseDef.Module] = _ => quotes.reflectV2.CaseDef

  def unapply(x: CaseDef): (Tree, Option[Term], Term) = (x.pattern, x.guard, x.rhs)

  trait Module private[compiletime] () {
    def apply(pattern: Tree, guard: Option[Term], rhs: Term): CaseDef
    def make(pattern: Tree, guard: Option[Term], rhs: Term): CaseDef
  }

}

/////// TypeCaseDef ///////////////////////////////////////////////////////////////

/** Branch of a type pattern match. */
trait TypeCaseDef private[compiletime] () extends Tree {
  def pattern: TypeTree
  def rhs: TypeTree
}
object TypeCaseDef {

  def quoted(using quotes: Quotes): TypeCaseDef.Module = quotes.reflectV2.TypeCaseDef
  given moduleConversion: (quotes: Quotes) => Conversion[TypeCaseDef.type, TypeCaseDef.Module] = _ => quotes.reflectV2.TypeCaseDef

  def unapply(tree: TypeCaseDef): (TypeTree, TypeTree) = (tree.pattern, tree.rhs)

  trait Module private[compiletime] () {
    def apply(pattern: TypeTree, rhs: TypeTree): TypeCaseDef
    def make(pattern: TypeTree, rhs: TypeTree): TypeCaseDef
  }

}

/////// Bind ///////////////////////////////////////////////////////////////

/** Pattern representing a `_ @ _` binding. */
trait Bind private[compiletime] () extends Tree {
  def name: String
  def pattern: Tree
}
object Bind {

  def quoted(using quotes: Quotes): Bind.Module = quotes.reflectV2.Bind
  given moduleConversion: (quotes: Quotes) => Conversion[Bind.type, Bind.Module] = _ => quotes.reflectV2.Bind

  def unapply(pattern: Bind): (String, Tree) = (pattern.name, pattern.pattern)

  trait Module private[compiletime] () {
    def apply(sym: Symbol, pattern: Tree): Bind
    def make(sym: Symbol, pattern: Tree): Bind
  }

}

/////// Unapply ///////////////////////////////////////////////////////////////

/** Pattern representing a `Xyz(...)` unapply. */
trait Unapply private[compiletime] () extends Tree {
  def fun: Term
  def implicits: List[Term]
  def patterns: List[Tree]
}
object Unapply {

  def quoted(using quotes: Quotes): Unapply.Module = quotes.reflectV2.Unapply
  given moduleConversion: (quotes: Quotes) => Conversion[Unapply.type, Unapply.Module] = _ => quotes.reflectV2.Unapply

  def unapply(x: Unapply): (Term, List[Term], List[Tree]) = (x.fun, x.implicits, x.patterns)

  trait Module private[compiletime] () {

    /** Creates an `Unapply` tree representing a pattern `<fun>(<patterns*>)(using <implicits*>)`. */
    def apply(fun: Term, implicits: List[Term], patterns: List[Tree]): Unapply

    /** Creates an `Unapply` tree representing a pattern `<fun>(<patterns*>)(using <implicits*>)`. */
    def make(fun: Term, implicits: List[Term], patterns: List[Tree]): Unapply
  }

}

/////// Alternatives ///////////////////////////////////////////////////////////////

/** Pattern representing `X | Y | ...` alternatives. */
trait Alternatives private[compiletime] () extends Tree {
  def patterns: List[Tree]
}
object Alternatives {

  def quoted(using quotes: Quotes): Alternatives.Module = quotes.reflectV2.Alternatives
  given moduleConversion: (quotes: Quotes) => Conversion[Alternatives.type, Alternatives.Module] = _ => quotes.reflectV2.Alternatives

  def unapply(x: Alternatives): Some[List[Tree]] = Some(x.patterns)

  trait Module private[compiletime] () {
    def apply(patterns: List[Tree]): Alternatives
    def make(patterns: List[Tree]): Alternatives
  }

}
