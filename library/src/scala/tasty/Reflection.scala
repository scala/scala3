package scala.tasty

import scala.internal.tasty.CompilerInterface

import scala.quoted.QuoteContext
import scala.quoted.show.SyntaxHighlight
import scala.tasty.reflect._

/** TASTy Reflect Interface.
 *
 *  Provides all functionality related with AST based metaprogramming.
 */
trait Reflection extends reflect.Types { reflectSelf: CompilerInterface =>

  ////////////////
  //   Owners   //
  ////////////////

  def rootOwner: Owner

  given Owner = rootOwner // TODO: Should be an implicit converion from QuoteContext to Context

  given OwnerOps as Owner.type = Owner

  /** Enclosing definition */
  object Owner:

    /** Create a new owner representing the given symbol as the encosing definition */
    def fromSymbol(sym: Symbol) = reflectSelf.Owner_fromSymbol(sym)

    /** The symbol of the current enclosing definition in scope */
    def current(using owner: Owner): Owner = owner

    /** The symbol of the owner */
    extension (self: Owner) def symbol = reflectSelf.Owner_symbol(self)

  end Owner

  ///////////////
  //   Source  //
  ///////////////

  object Source:

    /** Returns the source file being compiled. The path is relative to the current working directory. */
    def path: java.nio.file.Path = reflectSelf.Source_path

    /** Returns true if we've tried to reflect on a Java class. */
    def isJavaCompilationUnit: Boolean = reflectSelf.Source_isJavaCompilationUnit

    /** Returns true if we've tried to reflect on a Scala2 (non-Tasty) class. */
    def isScala2CompilationUnit: Boolean = reflectSelf.Source_isScala2CompilationUnit

    /** Returns true if we've tried to reflect on a class that's already loaded (e.g. Option). */
    def isAlreadyLoadedCompilationUnit: Boolean = reflectSelf.Source_isAlreadyLoadedCompilationUnit

    /** Class name of the current CompilationUnit */
    def compilationUnitClassname: String = reflectSelf.Source_compilationUnitClassname

  end Source


  ///////////////
  //   TREES   //
  ///////////////

  // ----- Tree -----------------------------------------------------

  given TreeOps as Tree.type = Tree

  object Tree:
    /** Members of Tree */
    extension (tree: Tree):
      /** Position in the source code */
      def pos: Position = reflectSelf.Tree_pos(tree)

      /** Symbol of defined or referred by this tree */
      def symbol: Symbol = reflectSelf.Tree_symbol(tree)

      /** Shows the tree as extractors */
      def showExtractors: String =
        new ExtractorsPrinter[reflectSelf.type](reflectSelf).showTree(tree)

      /** Shows the tree as fully typed source code */
      def show: String =
        tree.showWith(SyntaxHighlight.plain)

      /** Shows the tree as fully typed source code */
      def showWith(syntaxHighlight: SyntaxHighlight): String =
        new SourceCodePrinter[reflectSelf.type](reflectSelf)(syntaxHighlight).showTree(tree)

      /** Does this tree represent a valid expression? */
      def isExpr: Boolean =
        tree match
          case tree: Term =>
            tree.tpe.widen match
              case _: MethodType | _: PolyType => false
              case _ => true
          case _ => false

    end extension


    /** Convert this tree to an `quoted.Expr[T]` if the tree is a valid expression or throws */
    extension [T](tree: Tree)
      def asExprOf(using scala.quoted.Type[T])(using QuoteContext): scala.quoted.Expr[T] =
        if tree.isExpr then
          new scala.internal.quoted.Expr(tree, reflectSelf.compilerId).asExprOf[T]
        else tree match
          case tree: Term => throw new Exception("Expected an expression. This is a partially applied Term. Try eta-expanding the term first.")
          case _ => throw new Exception("Expected a Term but was: " + tree)

  end Tree

  given TypeTest[Tree, PackageClause] = reflectSelf.PackageClause_TypeTest

  given PackageClauseOps as PackageClause.type = PackageClause

  object PackageClause:
    def apply(pid: Ref, stats: List[Tree]): PackageClause =
      reflectSelf.PackageClause_apply(pid, stats)
    def copy(original: Tree)(pid: Ref, stats: List[Tree]): PackageClause =
      reflectSelf.PackageClause_copy(original)(pid, stats)
    def unapply(tree: PackageClause): Some[(Ref, List[Tree])] =
      Some((tree.pid, tree.stats))

    extension (self: PackageClause):
      def pid: Ref = reflectSelf.PackageClause_pid(self)
      def stats: List[Tree] = reflectSelf.PackageClause_stats(self)
    end extension
  end PackageClause


  given TypeTest[Tree, Import] = reflectSelf.Import_TypeTest

  given ImportOps as Import.type = Import

  object Import:
    def apply(expr: Term, selectors: List[ImportSelector]): Import =
      reflectSelf.Import_apply(expr, selectors)
    def copy(original: Tree)(expr: Term, selectors: List[ImportSelector]): Import =
      reflectSelf.Import_copy(original)(expr, selectors)
    def unapply(tree: Import): Option[(Term, List[ImportSelector])] =
      Some((tree.expr, tree.selectors))

    extension (self: Import):
      def expr: Term = reflectSelf.Import_expr(self)
      def selectors: List[ImportSelector] =
        reflectSelf.Import_selectors(self)
    end extension
  end Import


  given TypeTest[Tree, Statement] = reflectSelf.Statement_TypeTest

  // ----- Definitions ----------------------------------------------

  given TypeTest[Tree, Definition] = reflectSelf.Definition_TypeTest

  given DefinitionOps as Definition.type = Definition

  object Definition:
    extension (self: Definition):
      def name: String = reflectSelf.Definition_name(self)
    end extension
  end Definition

  // ClassDef

  given TypeTest[Tree, ClassDef] = reflectSelf.ClassDef_TypeTest

  given ClassDefOps as ClassDef.type = ClassDef

  object ClassDef:
    // TODO def apply(name: String, constr: DefDef, parents: List[TermOrTypeTree], selfOpt: Option[ValDef], body: List[Statement]): ClassDef
    def copy(original: Tree)(name: String, constr: DefDef, parents: List[Tree /* Term | TypeTree */], derived: List[TypeTree], selfOpt: Option[ValDef], body: List[Statement]): ClassDef =
      reflectSelf.ClassDef_copy(original)(name, constr, parents, derived, selfOpt, body)
    def unapply(cdef: ClassDef): Option[(String, DefDef, List[Tree /* Term | TypeTree */], List[TypeTree], Option[ValDef], List[Statement])] =
      Some((cdef.name, cdef.constructor, cdef.parents, cdef.derived, cdef.self, cdef.body))

    extension (self: ClassDef):
      def constructor: DefDef = reflectSelf.ClassDef_constructor(self)
      def parents: List[Tree /* Term | TypeTree */] = reflectSelf.ClassDef_parents(self)
      def derived: List[TypeTree] = reflectSelf.ClassDef_derived(self)
      def self: Option[ValDef] = reflectSelf.ClassDef_self(self)
      def body: List[Statement] = reflectSelf.ClassDef_body(self)
    end extension
  end ClassDef


  // DefDef

  given TypeTest[Tree, DefDef] = reflectSelf.DefDef_TypeTest

  given DefDefOps as DefDef.type = DefDef

  object DefDef:
    def apply(symbol: Symbol, rhsFn: List[Type] => List[List[Term]] => Option[Term]): DefDef =
      reflectSelf.DefDef_apply(symbol, rhsFn)
    def copy(original: Tree)(name: String, typeParams: List[TypeDef], paramss: List[List[ValDef]], tpt: TypeTree, rhs: Option[Term]): DefDef =
      reflectSelf.DefDef_copy(original)(name, typeParams, paramss, tpt, rhs)
    def unapply(ddef: DefDef): Option[(String, List[TypeDef], List[List[ValDef]], TypeTree, Option[Term])] =
      Some((ddef.name, ddef.typeParams, ddef.paramss, ddef.returnTpt, ddef.rhs))

    extension (self: DefDef):
      def typeParams: List[TypeDef] = reflectSelf.DefDef_typeParams(self)
      def paramss: List[List[ValDef]] = reflectSelf.DefDef_paramss(self)
      def returnTpt: TypeTree = reflectSelf.DefDef_returnTpt(self) // TODO rename to tpt
      def rhs: Option[Term] = reflectSelf.DefDef_rhs(self)
    end extension
  end DefDef


  // ValDef

  given TypeTest[Tree, ValDef] = reflectSelf.ValDef_TypeTest

  given ValDefOps as ValDef.type = ValDef

  object ValDef:
    def apply(symbol: Symbol, rhs: Option[Term]): ValDef =
      reflectSelf.ValDef_apply(symbol, rhs)
    def copy(original: Tree)(name: String, tpt: TypeTree, rhs: Option[Term]): ValDef =
      reflectSelf.ValDef_copy(original)(name, tpt, rhs)
    def unapply(vdef: ValDef): Option[(String, TypeTree, Option[Term])] =
      Some((vdef.name, vdef.tpt, vdef.rhs))

    extension (self: ValDef):
      def tpt: TypeTree = reflectSelf.ValDef_tpt(self)
      def rhs: Option[Term] = reflectSelf.ValDef_rhs(self)
    end extension
  end ValDef


  // TypeDef

  given TypeTest[Tree, TypeDef] = reflectSelf.TypeDef_TypeTest

  given TypeDefOps as TypeDef.type = TypeDef

  object TypeDef:
    def apply(symbol: Symbol): TypeDef =
      reflectSelf.TypeDef_apply(symbol)
    def copy(original: Tree)(name: String, rhs: Tree /*TypeTree | TypeBoundsTree*/): TypeDef =
      reflectSelf.TypeDef_copy(original)(name, rhs)
    def unapply(tdef: TypeDef): Option[(String, Tree /*TypeTree | TypeBoundsTree*/ /* TypeTree | TypeBoundsTree */)] =
      Some((tdef.name, tdef.rhs))

    extension (self: TypeDef):
      def rhs: Tree /*TypeTree | TypeBoundsTree*/ = reflectSelf.TypeDef_rhs(self)
    end extension
  end TypeDef

  // PackageDef

  given TypeTest[Tree, PackageDef] = reflectSelf.PackageDef_TypeTest

  given PackageDefOps as PackageDef.type = PackageDef

  object PackageDef:
    def unapply(tree: PackageDef): Option[(String, PackageDef)] =
      Some((tree.name, tree.owner))

    extension (self: PackageDef):
      def owner: PackageDef = reflectSelf.PackageDef_owner(self)
      def members: List[Statement] = reflectSelf.PackageDef_members(self)
    end extension
  end PackageDef

  // ----- Terms ----------------------------------------------------

  given TermOps as Term.type = Term

  object Term:
    extension (self: Term):

      /** Convert `Term` to an `quoted.Expr[Any]` if the term is a valid expression or throws */
      def seal: scala.quoted.Expr[Any] =
        if self.isExpr then new scala.internal.quoted.Expr(self, reflectSelf.compilerId)
        else throw new Exception("Cannot seal a partially applied Term. Try eta-expanding the term first.")

      /** Convert `Term` to an `quoted.Expr[Any]` if the term is a valid expression */
      def sealOpt: Option[scala.quoted.Expr[Any]] =
        if self.isExpr then Some(new scala.internal.quoted.Expr(self, reflectSelf.compilerId))
        else None

      /** Type of this term */
      def tpe: Type = reflectSelf.Term_tpe(self)

      /** Replace Inlined nodes and InlineProxy references to underlying arguments */
      def underlyingArgument: Term = reflectSelf.Term_underlyingArgument(self)

      /** Replace Ident nodes references to the underlying tree that defined them */
      def underlying: Term = reflectSelf.Term_underlying(self)

      /** Converts a partally applied term into a lambda expression */
      def etaExpand: Term = reflectSelf.Term_etaExpand(self)

      /** A unary apply node with given argument: `tree(arg)` */
      def appliedTo(arg: Term): Term =
        self.appliedToArgs(arg :: Nil)

      /** An apply node with given arguments: `tree(arg, args0, ..., argsN)` */
      def appliedTo(arg: Term, args: Term*): Term =
        self.appliedToArgs(arg :: args.toList)

      /** An apply node with given argument list `tree(args(0), ..., args(args.length - 1))` */
      def appliedToArgs(args: List[Term]): Apply =
        Apply(self, args)

      /** The current tree applied to given argument lists:
      *  `tree (argss(0)) ... (argss(argss.length -1))`
      */
      def appliedToArgss(argss: List[List[Term]]): Term =
        argss.foldLeft(self: Term)(Apply(_, _))

      /** The current tree applied to (): `tree()` */
      def appliedToNone: Apply =
        self.appliedToArgs(Nil)

      /** The current tree applied to given type argument: `tree[targ]` */
      def appliedToType(targ: Type): Term =
        self.appliedToTypes(targ :: Nil)

      /** The current tree applied to given type arguments: `tree[targ0, ..., targN]` */
      def appliedToTypes(targs: List[Type]): Term =
        self.appliedToTypeTrees(targs map (Inferred(_)))

      /** The current tree applied to given type argument list: `tree[targs(0), ..., targs(targs.length - 1)]` */
      def appliedToTypeTrees(targs: List[TypeTree]): Term =
        if (targs.isEmpty) self else TypeApply(self, targs)

      /** A select node that selects the given symbol.
      */
      def select(sym: Symbol): Select = Select(self, sym)

    end extension

  end Term

  given TypeTest[Tree, Term] = reflectSelf.Term_TypeTest

  given TypeTest[Tree, Ref] = reflectSelf.Ref_TypeTest

  object Ref:

    /** A tree representing the same reference as the given type */
    def term(tp: TermRef): Ref =
      reflectSelf.Ref_term(tp)

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
    def apply(sym: Symbol): Ref =
      reflectSelf.Ref_apply(sym)
  end Ref

  given TypeTest[Tree, Ident] = reflectSelf.Ident_TypeTest

  given IdentOps as Ident.type = Ident

  /** Scala term identifier */
  object Ident:
    def apply(tmref: TermRef): Term =
      reflectSelf.Ident_apply(tmref)

    def copy(original: Tree)(name: String): Ident =
      reflectSelf.Ident_copy(original)(name)

    /** Matches a term identifier and returns its name */
    def unapply(tree: Ident): Option[String] =
      Some(tree.name)

    extension (self: Ident):
      def name: String = reflectSelf.Ident_name(self)
    end extension
  end Ident

  given TypeTest[Tree, Select] = reflectSelf.Select_TypeTest
  given SelectOps as Select.type = Select

  /** Scala term selection */
  object Select:
    /** Select a term member by symbol */
    def apply(qualifier: Term, symbol: Symbol): Select =
      reflectSelf.Select_apply(qualifier, symbol)

    /** Select a field or a non-overloaded method by name
     *
     *  @note The method will produce an assertion error if the selected
     *        method is overloaded. The method `overloaded` should be used
     *        in that case.
     */
    def unique(qualifier: Term, name: String): Select =
      reflectSelf.Select_unique(qualifier, name)

    // TODO rename, this returns an Apply and not a Select
    /** Call an overloaded method with the given type and term parameters */
    def overloaded(qualifier: Term, name: String, targs: List[Type], args: List[Term]): Apply =
      reflectSelf.Select_overloaded(qualifier, name, targs, args)

    def copy(original: Tree)(qualifier: Term, name: String): Select =
      reflectSelf.Select_copy(original)(qualifier, name)

    /** Matches `<qualifier: Term>.<name: String>` */
    def unapply(x: Select): Option[(Term, String)] =
      Some((x.qualifier, x.name))

    extension (self: Select):
      def qualifier: Term = reflectSelf.Select_qualifier(self)
      def name: String = reflectSelf.Select_name(self)
      def signature: Option[Signature] = reflectSelf.Select_signature(self)
    end extension
  end Select

  given TypeTest[Tree, Literal] =
    reflectSelf.Literal_TypeTest

  given LiteralOps as Literal.type = Literal

  /** Scala literal constant */
  object Literal:

    /** Create a literal constant */
    def apply(constant: Constant): Literal =
      reflectSelf.Literal_apply(constant)

    def copy(original: Tree)(constant: Constant): Literal =
      reflectSelf.Literal_copy(original)(constant)

    /** Matches a literal constant */
    def unapply(x: Literal): Option[Constant] =
      Some(x.constant)

    extension (self: Literal):
      def constant: Constant = reflectSelf.Literal_constant(self)
    end extension
  end Literal

  given TypeTest[Tree, This] = reflectSelf.This_TypeTest

  given ThisOps as This.type = This

  /** Scala `this` or `this[id]` */
  object This:

    /** Create a `this[<id: Id]>` */
    def apply(cls: Symbol): This =
      reflectSelf.This_apply(cls)

    def copy(original: Tree)(qual: Option[Id]): This =
      reflectSelf.This_copy(original)(qual)

    /** Matches `this[<id: Option[Id]>` */
    def unapply(x: This): Option[Option[Id]] = Some(x.id)

    extension (self: This):
      def id: Option[Id] = reflectSelf.This_id(self)
    end extension
  end This

  given TypeTest[Tree, New] = reflectSelf.New_TypeTest

  given NewOps as New.type = New

  /** Scala `new` */
  object New:

    /** Create a `new <tpt: TypeTree>` */
    def apply(tpt: TypeTree): New =
      reflectSelf.New_apply(tpt)

    def copy(original: Tree)(tpt: TypeTree): New =
      reflectSelf.New_copy(original)(tpt)

    /** Matches a `new <tpt: TypeTree>` */
    def unapply(x: New): Option[TypeTree] = Some(x.tpt)

    extension (self: New):
      def tpt: TypeTree = reflectSelf.New_tpt(self)
    end extension
  end New

  given TypeTest[Tree, NamedArg] = reflectSelf.NamedArg_TypeTest

  given NamedArgOps as NamedArg.type = NamedArg

  /** Scala named argument `x = y` in argument position */
  object NamedArg:

    /** Create a named argument `<name: String> = <value: Term>` */
    def apply(name: String, arg: Term): NamedArg =
      reflectSelf.NamedArg_apply(name, arg)

    def copy(original: Tree)(name: String, arg: Term): NamedArg =
      reflectSelf.NamedArg_copy(original)(name, arg)

    /** Matches a named argument `<name: String> = <value: Term>` */
    def unapply(x: NamedArg): Option[(String, Term)] =
      Some((x.name, x.value))

    extension (self: NamedArg):
      def name: String = reflectSelf.NamedArg_name(self)
      def value: Term = reflectSelf.NamedArg_value(self)
    end extension
  end NamedArg

  given TypeTest[Tree, Apply] = reflectSelf.Apply_TypeTest

  given ApplyOps as Apply.type = Apply

  /** Scala parameter application */
  object Apply:

    /** Create a function application `<fun: Term>(<args: List[Term]>)` */
    def apply(fun: Term, args: List[Term]): Apply =
      reflectSelf.Apply_apply(fun, args)

    def copy(original: Tree)(fun: Term, args: List[Term]): Apply =
      reflectSelf.Apply_copy(original)(fun, args)

    /** Matches a function application `<fun: Term>(<args: List[Term]>)` */
    def unapply(x: Apply): Option[(Term, List[Term])] =
      Some((x.fun, x.args))

    extension (self: Apply):
      def fun: Term = reflectSelf.Apply_fun(self)
      def args: List[Term] = reflectSelf.Apply_args(self)
    end extension
  end Apply

  given TypeTest[Tree, TypeApply] = reflectSelf.TypeApply_TypeTest

  given TypeApplyOps as TypeApply.type = TypeApply

  /** Scala type parameter application */
  object TypeApply:

    /** Create a function type application `<fun: Term>[<args: List[TypeTree]>]` */
    def apply(fun: Term, args: List[TypeTree]): TypeApply =
      reflectSelf.TypeApply_apply(fun, args)

    def copy(original: Tree)(fun: Term, args: List[TypeTree]): TypeApply =
      reflectSelf.TypeApply_copy(original)(fun, args)

    /** Matches a function type application `<fun: Term>[<args: List[TypeTree]>]` */
    def unapply(x: TypeApply): Option[(Term, List[TypeTree])] =
      Some((x.fun, x.args))

    extension (self: TypeApply):
      def fun: Term = reflectSelf.TypeApply_fun(self)
      def args: List[TypeTree] = reflectSelf.TypeApply_args(self)
    end extension
  end TypeApply

  given TypeTest[Tree, Super] = reflectSelf.Super_TypeTest

  given SuperOps as Super.type = Super

  /** Scala `x.super` or `x.super[id]` */
  object Super:

    /** Creates a `<qualifier: Term>.super[<id: Option[Id]>` */
    def apply(qual: Term, mix: Option[Id]): Super =
      reflectSelf.Super_apply(qual, mix)

    def copy(original: Tree)(qual: Term, mix: Option[Id]): Super =
      reflectSelf.Super_copy(original)(qual, mix)

    /** Matches a `<qualifier: Term>.super[<id: Option[Id]>` */
    def unapply(x: Super): Option[(Term, Option[Id])] =
      Some((x.qualifier, x.id))

    extension (self: Super):
      def qualifier: Term = reflectSelf.Super_qualifier(self)
      def id: Option[Id] = reflectSelf.Super_id(self)
    end extension
  end Super


  given TypeTest[Tree, Typed] = reflectSelf.Typed_TypeTest

  given TypedOps as Typed.type = Typed

  /** Scala ascription `x: T` */
  object Typed:

    /** Create a type ascription `<x: Term>: <tpt: TypeTree>` */
    def apply(expr: Term, tpt: TypeTree): Typed =
      reflectSelf.Typed_apply(expr, tpt)

    def copy(original: Tree)(expr: Term, tpt: TypeTree): Typed =
      reflectSelf.Typed_copy(original)(expr, tpt)

    /** Matches `<expr: Term>: <tpt: TypeTree>` */
    def unapply(x: Typed): Option[(Term, TypeTree)] =
      Some((x.expr, x.tpt))

    extension (self: Typed):
      def expr: Term = reflectSelf.Typed_expr(self)
      def tpt: TypeTree = reflectSelf.Typed_tpt(self)
    end extension
  end Typed


  given TypeTest[Tree, Assign] = reflectSelf.Assign_TypeTest

  given AssignOps as Assign.type = Assign

  /** Scala assign `x = y` */
  object Assign:

    /** Create an assignment `<lhs: Term> = <rhs: Term>` */
    def apply(lhs: Term, rhs: Term): Assign =
      reflectSelf.Assign_apply(lhs, rhs)

    def copy(original: Tree)(lhs: Term, rhs: Term): Assign =
      reflectSelf.Assign_copy(original)(lhs, rhs)

    /** Matches an assignment `<lhs: Term> = <rhs: Term>` */
    def unapply(x: Assign): Option[(Term, Term)] =
      Some((x.lhs, x.rhs))

    extension (self: Assign):
      def lhs: Term = reflectSelf.Assign_lhs(self)
      def rhs: Term = reflectSelf.Assign_rhs(self)
    end extension
  end Assign


  given TypeTest[Tree, Block] = reflectSelf.Block_TypeTest

  given BlockOps as Block.type = Block

  /** Scala code block `{ stat0; ...; statN; expr }` term */
  object Block:

    /** Creates a block `{ <statements: List[Statement]>; <expr: Term> }` */
    def apply(stats: List[Statement], expr: Term): Block =
      reflectSelf.Block_apply(stats, expr)

    def copy(original: Tree)(stats: List[Statement], expr: Term): Block =
      reflectSelf.Block_copy(original)(stats, expr)

    /** Matches a block `{ <statements: List[Statement]>; <expr: Term> }` */
    def unapply(x: Block): Option[(List[Statement], Term)] =
      Some((x.statements, x.expr))

    extension (self: Block):
      def statements: List[Statement] = reflectSelf.Block_statements(self)
      def expr: Term = reflectSelf.Block_expr(self)
    end extension
  end Block


  given TypeTest[Tree, Closure] = reflectSelf.Closure_TypeTest

  given ClosureOps as Closure.type = Closure

  object Closure:

    def apply(meth: Term, tpt: Option[Type]): Closure =
      reflectSelf.Closure_apply(meth, tpt)

    def copy(original: Tree)(meth: Tree, tpt: Option[Type]): Closure =
      reflectSelf.Closure_copy(original)(meth, tpt)

    def unapply(x: Closure): Option[(Term, Option[Type])] =
      Some((x.meth, x.tpeOpt))

    extension (self: Closure):
      def meth: Term = reflectSelf.Closure_meth(self)
      def tpeOpt: Option[Type] = reflectSelf.Closure_tpeOpt(self)
    end extension
  end Closure


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
  object Lambda:
    def unapply(tree: Block): Option[(List[ValDef], Term)] = tree match {
      case Block((ddef @ DefDef(_, _, params :: Nil, _, Some(body))) :: Nil, Closure(meth, _))
      if ddef.symbol == meth.symbol =>
        Some(params, body)

      case _ => None
    }

    def apply(tpe: MethodType, rhsFn: List[Tree] => Tree): Block =
      reflectSelf.Lambda_apply(tpe, rhsFn)

  end Lambda

  given TypeTest[Tree, If] = reflectSelf.If_TypeTest

  given IfOps as If.type = If

  /** Scala `if`/`else` term */
  object If:

    /** Create an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>` */
    def apply(cond: Term, thenp: Term, elsep: Term): If =
      reflectSelf.If_apply(cond, thenp, elsep)

    def copy(original: Tree)(cond: Term, thenp: Term, elsep: Term): If =
      reflectSelf.If_copy(original)(cond, thenp, elsep)

    /** Matches an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>` */
    def unapply(tree: If): Option[(Term, Term, Term)] =
      Some((tree.cond, tree.thenp, tree.elsep))

    extension (self: If):
      def cond: Term = reflectSelf.If_cond(self)
      def thenp: Term = reflectSelf.If_thenp(self)
      def elsep: Term = reflectSelf.If_elsep(self)
    end extension
  end If

  given TypeTest[Tree, Match] = reflectSelf.Match_TypeTest

  given MatchOps as Match.type = Match

  /** Scala `match` term */
  object Match:

    /** Creates a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }` */
    def apply(selector: Term, cases: List[CaseDef]): Match =
      reflectSelf.Match_apply(selector, cases)

    def copy(original: Tree)(selector: Term, cases: List[CaseDef]): Match =
      reflectSelf.Match_copy(original)(selector, cases)

    /** Matches a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }` */
    def unapply(x: Match): Option[(Term, List[CaseDef])] =
      Some((x.scrutinee, x.cases))

    extension (self: Match):
      def scrutinee: Term = reflectSelf.Match_scrutinee(self)
      def cases: List[CaseDef] = reflectSelf.Match_cases(self)
    end extension
  end Match


  given TypeTest[Tree, GivenMatch] = reflectSelf.GivenMatch_TypeTest

  given GivenMatchOps as GivenMatch.type = GivenMatch

  /** Scala implicit `match` term */
  object GivenMatch:

    /** Creates a pattern match `given match { <cases: List[CaseDef]> }` */
    def apply(cases: List[CaseDef]): GivenMatch =
      reflectSelf.GivenMatch_apply(cases)

    def copy(original: Tree)(cases: List[CaseDef]): GivenMatch =
      reflectSelf.GivenMatch_copy(original)(cases)

    /** Matches a pattern match `given match { <cases: List[CaseDef]> }` */
    def unapply(x: GivenMatch): Option[List[CaseDef]] = Some(x.cases)


    extension (self: GivenMatch):
      def cases: List[CaseDef] = reflectSelf.GivenMatch_cases(self)
    end extension
  end GivenMatch


  given TypeTest[Tree, Try] = reflectSelf.Try_TypeTest

  given TryOps as Try.type = Try

  /** Scala `try`/`catch`/`finally` term */
  object Try:

    /** Create a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>` */
    def apply(expr: Term, cases: List[CaseDef], finalizer: Option[Term]): Try =
      reflectSelf.Try_apply(expr, cases, finalizer)

    def copy(original: Tree)(expr: Term, cases: List[CaseDef], finalizer: Option[Term]): Try =
      reflectSelf.Try_copy(original)(expr, cases, finalizer)

    /** Matches a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>` */
    def unapply(x: Try): Option[(Term, List[CaseDef], Option[Term])] =
      Some((x.body, x.cases, x.finalizer))

    extension (self: Try):
      def body: Term = reflectSelf.Try_body(self)
      def cases: List[CaseDef] = reflectSelf.Try_cases(self)
      def finalizer: Option[Term] = reflectSelf.Try_finalizer(self)
    end extension
  end Try


  given TypeTest[Tree, Return] = reflectSelf.Return_TypeTest

  given ReturnOps as Return.type = Return

  /** Scala local `return` */
  object Return:

    /** Creates `return <expr: Term>` */
    def apply(expr: Term): Return =
      reflectSelf.Return_apply(expr)

    def copy(original: Tree)(expr: Term): Return =
      reflectSelf.Return_copy(original)(expr)

    /** Matches `return <expr: Term>` */
    def unapply(x: Return): Option[Term] = Some(x.expr)

    extension (self: Return):
      def expr: Term = reflectSelf.Return_expr(self)
    end extension
  end Return


  given TypeTest[Tree, Repeated] = reflectSelf.Repeated_TypeTest

  given RepeatedOps as Repeated.type = Repeated

  object Repeated:

    def apply(elems: List[Term], tpt: TypeTree): Repeated =
      reflectSelf.Repeated_apply(elems, tpt)

    def copy(original: Tree)(elems: List[Term], tpt: TypeTree): Repeated =
      reflectSelf.Repeated_copy(original)(elems, tpt)

    def unapply(x: Repeated): Option[(List[Term], TypeTree)] =
      Some((x.elems, x.elemtpt))

    extension (self: Repeated):
      def elems: List[Term] = reflectSelf.Repeated_elems(self)
      def elemtpt: TypeTree = reflectSelf.Repeated_elemtpt(self)
    end extension
  end Repeated


  given TypeTest[Tree, Inlined] = reflectSelf.Inlined_TypeTest

  given InlinedOps as Inlined.type = Inlined

  object Inlined:

    def apply(call: Option[Tree /* Term | TypeTree */], bindings: List[Definition], expansion: Term): Inlined =
      reflectSelf.Inlined_apply(call, bindings, expansion)

    def copy(original: Tree)(call: Option[Tree /* Term | TypeTree */], bindings: List[Definition], expansion: Term): Inlined =
      reflectSelf.Inlined_copy(original)(call, bindings, expansion)

    def unapply(x: Inlined): Option[(Option[Tree /* Term | TypeTree */], List[Definition], Term)] =
      Some((x.call, x.bindings, x.body))

    extension (self: Inlined):
      def call: Option[Tree /* Term | TypeTree */] = reflectSelf.Inlined_call(self)
      def bindings: List[Definition] = reflectSelf.Inlined_bindings(self)
      def body: Term = reflectSelf.Inlined_body(self)
    end extension
  end Inlined


  given TypeTest[Tree, SelectOuter] = reflectSelf.SelectOuter_TypeTest

  given SelectOuterOps as SelectOuter.type = SelectOuter

  object SelectOuter:

    def apply(qualifier: Term, name: String, levels: Int): SelectOuter =
      reflectSelf.SelectOuter_apply(qualifier, name, levels)

    def copy(original: Tree)(qualifier: Term, name: String, levels: Int): SelectOuter =
      reflectSelf.SelectOuter_copy(original)(qualifier, name, levels)

    def unapply(x: SelectOuter): Option[(Term, Int, Type)] = // TODO homogenize order of parameters
      Some((x.qualifier, x.level, x.tpe))

    extension (self: SelectOuter):
      def qualifier: Term = reflectSelf.SelectOuter_qualifier(self)
      def level: Int = reflectSelf.SelectOuter_level(self)
    end extension
  end SelectOuter


  given TypeTest[Tree, While] = reflectSelf.While_TypeTest

  given WhileOps as While.type = While

  object While:

    /** Creates a while loop `while (<cond>) <body>` and returns (<cond>, <body>) */
    def apply(cond: Term, body: Term): While =
      reflectSelf.While_apply(cond, body)

    def copy(original: Tree)(cond: Term, body: Term): While =
      reflectSelf.While_copy(original)(cond, body)

    /** Extractor for while loops. Matches `while (<cond>) <body>` and returns (<cond>, <body>) */
    def unapply(x: While): Option[(Term, Term)] =
      Some((x.cond, x.body))

    extension (self: While):
      def cond: Term = reflectSelf.While_cond(self)
      def body: Term = reflectSelf.While_body(self)
    end extension
  end While


  // ----- TypeTrees ------------------------------------------------

  given TypeTest[Tree, TypeTree] = reflectSelf.TypeTree_TypeTest

  given TypeTreeOps as TypeTree.type = TypeTree

  object TypeTree:
    extension (self: TypeTree):
      /** Type of this type tree */
      def tpe: Type = reflectSelf.TypeTree_tpe(self)
    end extension
  end TypeTree

  given TypeTest[Tree, Inferred] = reflectSelf.Inferred_TypeTest

  /** TypeTree containing an inferred type */
  object Inferred:
    def apply(tpe: Type): Inferred =
      reflectSelf.Inferred_apply(tpe)
    /** Matches a TypeTree containing an inferred type */
    def unapply(x: Inferred): Boolean = true
  end Inferred

  given TypeTest[Tree, TypeIdent] = reflectSelf.TypeIdent_TypeTest

  given TypeIdentOps as TypeIdent.type = TypeIdent

  object TypeIdent:
    def apply(sym: Symbol): TypeTree =
      reflectSelf.TypeRef_apply(sym)
    def copy(original: Tree)(name: String): TypeIdent =
      reflectSelf.TypeIdent_copy(original)(name)
    def unapply(x: TypeIdent): Option[String] = Some(x.name)

    extension (self: TypeIdent):
      def name: String = reflectSelf.TypeIdent_name(self)
    end extension
  end TypeIdent

  given TypeTest[Tree, TypeSelect] = reflectSelf.TypeSelect_TypeTest

  given TypeSelectOps as TypeSelect.type = TypeSelect

  object TypeSelect:
    def apply(qualifier: Term, name: String): TypeSelect =
      reflectSelf.TypeSelect_apply(qualifier, name)
    def copy(original: Tree)(qualifier: Term, name: String): TypeSelect =
      reflectSelf.TypeSelect_copy(original)(qualifier, name)
    def unapply(x: TypeSelect): Option[(Term, String)] =
      Some((x.qualifier, x.name))

    extension (self: TypeSelect):
      def qualifier: Term = reflectSelf.TypeSelect_qualifier(self)
      def name: String = reflectSelf.TypeSelect_name(self)
    end extension
  end TypeSelect


  given TypeTest[Tree, Projection] = reflectSelf.Projection_TypeTest

  given ProjectionOps as Projection.type = Projection

  object Projection:
    // TODO def apply(qualifier: TypeTree, name: String): Project
    def copy(original: Tree)(qualifier: TypeTree, name: String): Projection =
      reflectSelf.Projection_copy(original)(qualifier, name)
    def unapply(x: Projection): Option[(TypeTree, String)] =
      Some((x.qualifier, x.name))

    extension (self: Projection):
      def qualifier: TypeTree = reflectSelf.Projection_qualifier(self)
      def name: String = reflectSelf.Projection_name(self)
    end extension
  end Projection


  given TypeTest[Tree, Singleton] = reflectSelf.Singleton_TypeTest

  given SingletonOps as Singleton.type = Singleton

  object Singleton:
    def apply(ref: Term): Singleton =
      reflectSelf.Singleton_apply(ref)
    def copy(original: Tree)(ref: Term): Singleton =
      reflectSelf.Singleton_copy(original)(ref)
    def unapply(x: Singleton): Option[Term] =
      Some(x.ref)

    extension (self: Singleton):
      def ref: Term = reflectSelf.Singleton_ref(self)
    end extension
  end Singleton


  given TypeTest[Tree, Refined] = reflectSelf.Refined_TypeTest

  given RefinedOps as Refined.type = Refined

  object Refined:
    // TODO def apply(tpt: TypeTree, refinements: List[Definition]): Refined
    def copy(original: Tree)(tpt: TypeTree, refinements: List[Definition]): Refined =
      reflectSelf.Refined_copy(original)(tpt, refinements)
    def unapply(x: Refined): Option[(TypeTree, List[Definition])] =
      Some((x.tpt, x.refinements))

    extension (self: Refined):
      def tpt: TypeTree = reflectSelf.Refined_tpt(self)
      def refinements: List[Definition] = reflectSelf.Refined_refinements(self)
    end extension
  end Refined


  given TypeTest[Tree, Applied] = reflectSelf.Applied_TypeTest

  given AppliedOps as Applied.type = Applied

  object Applied:
    def apply(tpt: TypeTree, args: List[Tree /*TypeTree | TypeBoundsTree*/]): Applied =
      reflectSelf.Applied_apply(tpt, args)
    def copy(original: Tree)(tpt: TypeTree, args: List[Tree /*TypeTree | TypeBoundsTree*/]): Applied =
      reflectSelf.Applied_copy(original)(tpt, args)
    def unapply(x: Applied): Option[(TypeTree, List[Tree /*TypeTree | TypeBoundsTree*/])] =
      Some((x.tpt, x.args))

    extension (self: Applied):
      def tpt: TypeTree = reflectSelf.Applied_tpt(self)
      def args: List[Tree /*TypeTree | TypeBoundsTree*/] = reflectSelf.Applied_args(self)
    end extension
  end Applied


  given TypeTest[Tree, Annotated] =
    reflectSelf.Annotated_TypeTest

  given AnnotatedOps as Annotated.type = Annotated

  object Annotated:
    def apply(arg: TypeTree, annotation: Term): Annotated =
      reflectSelf.Annotated_apply(arg, annotation)
    def copy(original: Tree)(arg: TypeTree, annotation: Term): Annotated =
      reflectSelf.Annotated_copy(original)(arg, annotation)
    def unapply(x: Annotated): Option[(TypeTree, Term)] =
      Some((x.arg, x.annotation))

    extension (self: Annotated):
      def arg: TypeTree = reflectSelf.Annotated_arg(self)
      def annotation: Term = reflectSelf.Annotated_annotation(self)
    end extension
  end Annotated


  given TypeTest[Tree, MatchTypeTree] =
    reflectSelf.MatchTypeTree_TypeTest

  given MatchTypeTreeOps as MatchTypeTree.type = MatchTypeTree

  object MatchTypeTree:
    def apply(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef]): MatchTypeTree =
      reflectSelf.MatchTypeTree_apply(bound, selector, cases)
    def copy(original: Tree)(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef]): MatchTypeTree =
      reflectSelf.MatchTypeTree_copy(original)(bound, selector, cases)
    def unapply(x: MatchTypeTree): Option[(Option[TypeTree], TypeTree, List[TypeCaseDef])] =
      Some((x.bound, x.selector, x.cases))

    extension (self: MatchTypeTree):
      def bound: Option[TypeTree] = reflectSelf.MatchTypeTree_bound(self)
      def selector: TypeTree = reflectSelf.MatchTypeTree_selector(self)
      def cases: List[TypeCaseDef] = reflectSelf.MatchTypeTree_cases(self)
    end extension
  end MatchTypeTree


  given TypeTest[Tree, ByName] =
    reflectSelf.ByName_TypeTest

  given ByNameOps as ByName.type = ByName

  object ByName:
    def apply(result: TypeTree): ByName =
      reflectSelf.ByName_apply(result)
    def copy(original: Tree)(result: TypeTree): ByName =
      reflectSelf.ByName_copy(original)(result)
    def unapply(x: ByName): Option[TypeTree] =
      Some(x.result)

    extension (self: ByName):
      def result: TypeTree = reflectSelf.ByName_result(self)
    end extension
  end ByName


  given TypeTest[Tree, LambdaTypeTree] = reflectSelf.LambdaTypeTree_TypeTest

  given LambdaTypeTreeOps as LambdaTypeTree.type = LambdaTypeTree

  object LambdaTypeTree:
    def apply(tparams: List[TypeDef], body: Tree /*TypeTree | TypeBoundsTree*/): LambdaTypeTree =
      reflectSelf.Lambdaapply(tparams, body)
    def copy(original: Tree)(tparams: List[TypeDef], body: Tree /*TypeTree | TypeBoundsTree*/): LambdaTypeTree =
      reflectSelf.Lambdacopy(original)(tparams, body)
    def unapply(tree: LambdaTypeTree): Option[(List[TypeDef], Tree /*TypeTree | TypeBoundsTree*/)] =
      Some((tree.tparams, tree.body))

    extension (self: LambdaTypeTree):
      def tparams: List[TypeDef] = reflectSelf.Lambdatparams(self)
      def body: Tree /*TypeTree | TypeBoundsTree*/ = reflectSelf.Lambdabody(self)
    end extension
  end LambdaTypeTree


  given TypeTest[Tree, TypeBind] = reflectSelf.TypeBind_TypeTest

  given TypeBindOps as TypeBind.type = TypeBind

  object TypeBind:
    // TODO def apply(name: String, tree: Tree): TypeBind
    def copy(original: Tree)(name: String, tpt: Tree /*TypeTree | TypeBoundsTree*/): TypeBind =
      reflectSelf.TypeBind_copy(original)(name, tpt)
    def unapply(x: TypeBind): Option[(String, Tree /*TypeTree | TypeBoundsTree*/)] =
      Some((x.name, x.body))

    extension (self: TypeBind):
      def name: String = reflectSelf.TypeBind_name(self)
      def body: Tree /*TypeTree | TypeBoundsTree*/ = reflectSelf.TypeBind_body(self)
    end extension
  end TypeBind


  given TypeTest[Tree, TypeBlock] = reflectSelf.TypeBlock_TypeTest

  given TypeBlockOps as TypeBlock.type = TypeBlock

  object TypeBlock:
    def apply(aliases: List[TypeDef], tpt: TypeTree): TypeBlock =
      reflectSelf.TypeBlock_apply(aliases, tpt)
    def copy(original: Tree)(aliases: List[TypeDef], tpt: TypeTree): TypeBlock =
      reflectSelf.TypeBlock_copy(original)(aliases, tpt)
    def unapply(x: TypeBlock): Option[(List[TypeDef], TypeTree)] =
      Some((x.aliases, x.tpt))

    extension (self: TypeBlock):
      def aliases: List[TypeDef] = reflectSelf.TypeBlock_aliases(self)
      def tpt: TypeTree = reflectSelf.TypeBlock_tpt(self)
    end extension
  end TypeBlock


  // ----- TypeBoundsTrees ------------------------------------------------

  given TypeTest[Tree, TypeBoundsTree] = reflectSelf.TypeBoundsTree_TypeTest

  given TypeBoundsTreeOps as TypeBoundsTree.type = TypeBoundsTree

  object TypeBoundsTree:
    def unapply(x: TypeBoundsTree): Option[(TypeTree, TypeTree)] =
      Some((x.low, x.hi))

    extension (self: TypeBoundsTree):
      def tpe: TypeBounds = reflectSelf.TypeBoundsTree_tpe(self)
      def low: TypeTree = reflectSelf.TypeBoundsTree_low(self)
      def hi: TypeTree = reflectSelf.TypeBoundsTree_hi(self)
    end extension
  end TypeBoundsTree


  given TypeTest[Tree, WildcardTypeTree] = reflectSelf.WildcardTypeTree_TypeTest

  given WildcardTypeTreeOps as WildcardTypeTree.type = WildcardTypeTree

  object WildcardTypeTree:
    /** Matches a TypeBoundsTree containing wildcard type bounds */
    def unapply(x: WildcardTypeTree): Boolean = true

    extension (self: WildcardTypeTree):
      def tpe: Type = reflectSelf.WildcardTypeTree_tpe(self)
    end extension
  end WildcardTypeTree

  // ----- CaseDefs ------------------------------------------------

  given TypeTest[Tree, CaseDef] = reflectSelf.CaseDef_TypeTest
  given CaseDefOps as CaseDef.type = CaseDef

  object CaseDef:
    def apply(pattern: Tree, guard: Option[Term], rhs: Term): CaseDef =
      reflectSelf.CaseDef_module_apply(pattern, guard, rhs)

    def copy(original: Tree)(pattern: Tree, guard: Option[Term], rhs: Term): CaseDef =
      reflectSelf.CaseDef_module_copy(original)(pattern, guard, rhs)

    def unapply(x: CaseDef): Option[(Tree, Option[Term], Term)] =
      Some((x.pattern, x.guard, x.rhs))

    extension (caseDef: CaseDef):
      def pattern: Tree = reflectSelf.CaseDef_pattern(caseDef)
      def guard: Option[Term] = reflectSelf.CaseDef_guard(caseDef)
      def rhs: Term = reflectSelf.CaseDef_rhs(caseDef)
    end extension
  end CaseDef


  given TypeTest[Tree, TypeCaseDef] =
    reflectSelf.TypeCaseDef_TypeTest

  given TypeCaseDefOps as TypeCaseDef.type = TypeCaseDef

  object TypeCaseDef:
    def apply(pattern: TypeTree, rhs: TypeTree): TypeCaseDef =
      reflectSelf.TypeCaseDef_module_apply(pattern, rhs)

    def copy(original: Tree)(pattern: TypeTree, rhs: TypeTree): TypeCaseDef =
      reflectSelf.TypeCaseDef_module_copy(original)(pattern, rhs)

    def unapply(tree: TypeCaseDef): Option[(TypeTree, TypeTree)] =
      Some((tree.pattern, tree.rhs))

    extension (caseDef: TypeCaseDef):
      def pattern: TypeTree = reflectSelf.TypeCaseDef_pattern(caseDef)
      def rhs: TypeTree = reflectSelf.TypeCaseDef_rhs(caseDef)
    end extension
  end TypeCaseDef

  // ----- Patterns ------------------------------------------------

  given TypeTest[Tree, Bind] = reflectSelf.Bind_TypeTest
  given BindOps as Bind.type = Bind

  object Bind:
    def apply(sym: Symbol, pattern: Tree): Bind =
      reflectSelf.Tree_Bind_module_apply(sym, pattern)
    def copy(original: Tree)(name: String, pattern: Tree): Bind =
      reflectSelf.Tree_Bind_module_copy(original)(name, pattern)
    def unapply(pattern: Bind): Option[(String, Tree)] =
      Some((pattern.name, pattern.pattern))

    extension (bind: Bind):
      def name: String = reflectSelf.Tree_Bind_name(bind)
      def pattern: Tree = reflectSelf.Tree_Bind_pattern(bind)
    end extension
  end Bind


  given TypeTest[Tree, Unapply] = reflectSelf.Unapply_TypeTest
  given UnapplyOps as Unapply.type = Unapply

  object Unapply:
    // TODO def apply(fun: Term, implicits: List[Term], patterns: List[Tree]): Unapply
    def copy(original: Tree)(fun: Term, implicits: List[Term], patterns: List[Tree]): Unapply =
      reflectSelf.Tree_Unapply_module_copy(original)(fun, implicits, patterns)
    def unapply(x: Unapply): Option[(Term, List[Term], List[Tree])] =
      Some((x.fun, x.implicits, x.patterns))

    extension (unapply: Unapply):
      def fun: Term = reflectSelf.Tree_Unapply_fun(unapply)
      def implicits: List[Term] = reflectSelf.Tree_Unapply_implicits(unapply)
      def patterns: List[Tree] = reflectSelf.Tree_Unapply_patterns(unapply)
    end extension
  end Unapply


  given TypeTest[Tree, Alternatives] = reflectSelf.Alternatives_TypeTest
  given AlternativesOps as Alternatives.type = Alternatives

  object Alternatives:
    def apply(patterns: List[Tree]): Alternatives =
      reflectSelf.Tree_Alternatives_module_apply(patterns)
    def copy(original: Tree)(patterns: List[Tree]): Alternatives =
      reflectSelf.Tree_Alternatives_module_copy(original)(patterns)
    def unapply(x: Alternatives): Option[List[Tree]] =
      Some(x.patterns)

    extension (alternatives: Alternatives):
      def patterns: List[Tree] = reflectSelf.Tree_Alternatives_patterns(alternatives)
    end extension
  end Alternatives



  //////////////////////
  // IMPORT SELECTORS //
  /////////////////////

  given TypeTest[ImportSelector, SimpleSelector] = reflectSelf.SimpleSelector_TypeTest
  given SimpleSelectorOps as SimpleSelector.type = SimpleSelector

  object SimpleSelector:
    def unapply(x: SimpleSelector): Option[Id] = Some(x.selection)

    extension (self: SimpleSelector):
      def selection: Id =
        reflectSelf.SimpleSelector_selection(self)
    end extension
  end SimpleSelector


  given TypeTest[ImportSelector, RenameSelector] = reflectSelf.RenameSelector_TypeTest
  given RenameSelectorOps as RenameSelector.type = RenameSelector

  object RenameSelector:
    def unapply(x: RenameSelector): Option[(Id, Id)] = Some((x.from, x.to))
    extension (self: RenameSelector):
      def from: Id =
        reflectSelf.RenameSelector_from(self)

      def to: Id =
        reflectSelf.RenameSelector_to(self)
  end RenameSelector


  given TypeTest[ImportSelector, OmitSelector] = reflectSelf.OmitSelector_TypeTest
  given OmitSelectorOmitSelectorOps as OmitSelector.type = OmitSelector

  object OmitSelector:
    def unapply(x: OmitSelector): Option[Id] = Some(x.omitted)

    extension (self: OmitSelector):
      def omitted: Id =
        reflectSelf.SimpleSelector_omitted(self)
  end OmitSelector


  ///////////////
  //   TYPES   //
  ///////////////

  /** Returns the type (Type) of T */
  def typeOf[T](using qtype: scala.quoted.Type[T]): Type =
    qtype.asInstanceOf[scala.internal.quoted.Type[T]].typeTree.asInstanceOf[TypeTree].tpe


  // ----- Types ----------------------------------------------------

  given TypeTest[Type, Type] = reflectSelf.Type_TypeTest
  given TypeOps as Type.type = Type

  object Type:

    def apply(clazz: Class[_]): Type =
      reflectSelf.Type_apply(clazz)

    extension (self: Type):

      /** Shows the tree as extractors */
      def showExtractors: String =
        new ExtractorsPrinter[reflectSelf.type](reflectSelf).showType(self)

      /** Shows the tree as fully typed source code */
      def show: String =
        self.showWith(SyntaxHighlight.plain)

      /** Shows the tree as fully typed source code */
      def showWith(syntaxHighlight: SyntaxHighlight): String =
        new SourceCodePrinter[reflectSelf.type](reflectSelf)(syntaxHighlight).showType(self)

      /** Convert `Type` to an `quoted.Type[_]` */
      def seal: scala.quoted.Type[_] =
        new scala.internal.quoted.Type(Inferred(self), reflectSelf.compilerId)

      /** Is `self` type the same as `that` type?
       *  This is the case iff `self <:< that` and `that <:< self`.
       */
      def =:=(that: Type): Boolean = reflectSelf.Type_isTypeEq(self)(that)

      /** Is this type a subtype of that type? */
      def <:<(that: Type): Boolean = reflectSelf.Type_isSubType(self)(that)

      /** Widen from singleton type to its underlying non-singleton
       *  base type by applying one or more `underlying` dereferences,
       *  Also go from => T to T.
       *  Identity for all other types. Example:
       *
       *  class Outer { class C ; val x: C }
       *  def o: Outer
       *  <o.x.type>.widen = o.C
       */
      def widen: Type = reflectSelf.Type_widen(self)

      /** Widen from TermRef to its underlying non-termref
        *  base type, while also skipping `=>T` types.
        */
      def widenTermRefExpr: Type = reflectSelf.Type_widenTermRefExpr(self)

      /** Follow aliases and dereferences LazyRefs, annotated types and instantiated
        *  TypeVars until type is no longer alias type, annotated type, LazyRef,
        *  or instantiated type variable.
        */
      def dealias: Type = reflectSelf.Type_dealias(self)

      /** A simplified version of this type which is equivalent wrt =:= to this type.
       *  Reduces typerefs, applied match types, and and or types.
       */
      def simplified: Type = reflectSelf.Type_simplified(self)

      def classSymbol: Option[Symbol] = reflectSelf.Type_classSymbol(self)
      def typeSymbol: Symbol = reflectSelf.Type_typeSymbol(self)
      def termSymbol: Symbol = reflectSelf.Type_termSymbol(self)
      def isSingleton: Boolean = reflectSelf.Type_isSingleton(self)
      def memberType(member: Symbol): Type = reflectSelf.Type_memberType(self)(member)

      /** The base classes of this type with the class itself as first element. */
      def baseClasses: List[Symbol] = reflectSelf.Type_baseClasses(self)

      /** The least type instance of given class which is a super-type
       *  of this type.  Example:
       *  {{{
       *    class D[T]
       *    class C extends p.D[Int]
       *    ThisType(C).baseType(D) = p.D[Int]
       * }}}
       */
      def baseType(cls: Symbol): Type = reflectSelf.Type_baseType(self)(cls)

      /** Is this type an instance of a non-bottom subclass of the given class `cls`? */
      def derivesFrom(cls: Symbol): Boolean =
        reflectSelf.Type_derivesFrom(self)(cls)

      /** Is this type a function type?
       *
       *  @return true if the dealised type of `self` without refinement is `FunctionN[T1, T2, ..., Tn]`
       *
       *  @note The function
       *
       *     - returns true for `given Int => Int` and `erased Int => Int`
       *     - returns false for `List[Int]`, despite that `List[Int] <:< Int => Int`.
       */
      def isFunctionType: Boolean = reflectSelf.Type_isFunctionType(self)

      /** Is this type an context function type?
       *
       *  @see `isFunctionType`
       */
      def isContextFunctionType: Boolean = reflectSelf.Type_isContextFunctionType(self)

      /** Is this type an erased function type?
       *
       *  @see `isFunctionType`
       */
      def isErasedFunctionType: Boolean = reflectSelf.Type_isErasedFunctionType(self)

      /** Is this type a dependent function type?
       *
       *  @see `isFunctionType`
       */
      def isDependentFunctionType: Boolean = reflectSelf.Type_isDependentFunctionType(self)

      /** The type <this . sym>, reduced if possible */
      def select(sym: Symbol): Type = reflectSelf.Type_select(self)(sym)

      /** The current type applied to given type arguments: `this[targ]` */
      def appliedTo(targ: Type): Type = reflectSelf.Type_appliedTo(self)(List(targ))

      /** The current type applied to given type arguments: `this[targ0, ..., targN]` */
      def appliedTo(targs: List[Type]): Type = reflectSelf.Type_appliedTo(self)(targs)

    end extension
  end Type

  given TypeTest[Type, ConstantType] = reflectSelf.ConstantType_TypeTest
  given ConstantTypeOps as ConstantType.type = ConstantType

  object ConstantType:
    def apply(x : Constant): ConstantType = reflectSelf.ConstantType_apply(x)
    def unapply(x: ConstantType): Option[Constant] = Some(x.constant)

    extension (self: ConstantType):
      def constant: Constant = reflectSelf.ConstantType_constant(self)
    end extension
  end ConstantType


  given TypeTest[Type, TermRef] = reflectSelf.TermRef_TypeTest
  given TermRefOps as TermRef.type = TermRef

  object TermRef:
    def apply(qual: Type, name: String): TermRef =
      reflectSelf.TermRef_apply(qual, name)
    def unapply(x: TermRef): Option[(Type, String)] =
      Some((x.qualifier, x.name))

    extension (self: TermRef):
      def qualifier: Type = reflectSelf.TermRef_qualifier(self)
      def name: String = reflectSelf.TermRef_name(self)
    end extension
  end TermRef


  given TypeTest[Type, TypeRef] = reflectSelf.TypeRef_TypeTest
  given TypeRefOps as TypeRef.type = TypeRef

  object TypeRef:
    def unapply(x: TypeRef): Option[(Type, String)] =
      Some((x.qualifier, x.name))

    extension (self: TypeRef):
      def qualifier: Type = reflectSelf.TypeRef_qualifier(self)
      def name: String = reflectSelf.TypeRef_name(self)
      def isOpaqueAlias: Boolean = reflectSelf.TypeRef_isOpaqueAlias(self)
      def translucentSuperType: Type = reflectSelf.TypeRef_translucentSuperType(self)
    end extension
  end TypeRef


  given TypeTest[Type, SuperType] = reflectSelf.SuperType_TypeTest
  given SuperTypeOps as SuperType.type = SuperType

  object SuperType:
    def apply(thistpe: Type, supertpe: Type): SuperType =
      reflectSelf.SuperType_apply(thistpe, supertpe)

    def unapply(x: SuperType): Option[(Type, Type)] =
      Some((x.thistpe, x.supertpe))

    extension (self: SuperType):
      def thistpe: Type = reflectSelf.SuperType_thistpe(self)
      def supertpe: Type = reflectSelf.SuperType_supertpe(self)
    end extension
  end SuperType


  given TypeTest[Type, Refinement] = reflectSelf.Refinement_TypeTest
  given RefinementOps as Refinement.type = Refinement

  object Refinement:
    def apply(parent: Type, name: String, info: Type): Refinement =
      reflectSelf.Refinement_apply(parent, name, info)

    def unapply(x: Refinement): Option[(Type, String, Type)] =
      Some((x.parent, x.name, x.info))

    extension (self: Refinement):
      def parent: Type = reflectSelf.Refinement_parent(self)
      def name: String = reflectSelf.Refinement_name(self)
      def info: Type = reflectSelf.Refinement_info(self)
    end extension
  end Refinement


  given TypeTest[Type, AppliedType] = reflectSelf.AppliedType_TypeTest
  given AppliedTypeOps as AppliedType.type = AppliedType

  object AppliedType:
    def unapply(x: AppliedType): Option[(Type, List[Type])] =
      Some((x.tycon, x.args))

    extension (self: AppliedType):
      def tycon: Type = reflectSelf.AppliedType_tycon(self)
      def args: List[Type] = reflectSelf.AppliedType_args(self)
    end extension
  end AppliedType


  given TypeTest[Type, AnnotatedType] = reflectSelf.AnnotatedType_TypeTest
  given AnnotatedTypeOps as AnnotatedType.type = AnnotatedType

  object AnnotatedType:
    def apply(underlying: Type, annot: Term): AnnotatedType =
      reflectSelf.AnnotatedType_apply(underlying, annot)
    def unapply(x: AnnotatedType): Option[(Type, Term)] =
      Some((x.underlying, x.annot))

    extension (self: AnnotatedType):
      def underlying: Type = reflectSelf.AnnotatedType_underlying(self)
      def annot: Term = reflectSelf.AnnotatedType_annot(self)
    end extension
  end AnnotatedType


  given TypeTest[Type, AndType] = reflectSelf.AndType_TypeTest
  given AndTypeOps as AndType.type = AndType

  object AndType:
    def apply(lhs: Type, rhs: Type): AndType =
      reflectSelf.AndType_apply(lhs, rhs)
    def unapply(x: AndType): Option[(Type, Type)] =
      Some((x.left, x.right))

    extension (self: AndType):
      def left: Type = reflectSelf.AndType_left(self)
      def right: Type = reflectSelf.AndType_right(self)
    end extension
  end AndType


  given TypeTest[Type, OrType] = reflectSelf.OrType_TypeTest
  given OrTypeOps as OrType.type = OrType

  object OrType:
    def apply(lhs: Type, rhs: Type): OrType = reflectSelf.OrType_apply(lhs, rhs)
    def unapply(x: OrType): Option[(Type, Type)] =
      Some((x.left, x.right))

    extension (self: OrType):
      def left: Type = reflectSelf.OrType_left(self)
      def right: Type = reflectSelf.OrType_right(self)
    end extension
  end OrType


  given TypeTest[Type, MatchType] = reflectSelf.MatchType_TypeTest
  given MatchTypeOps as MatchType.type = MatchType

  object MatchType:
    def apply(bound: Type, scrutinee: Type, cases: List[Type]): MatchType =
      reflectSelf.MatchType_apply(bound, scrutinee, cases)
    def unapply(x: MatchType): Option[(Type, Type, List[Type])] =
      Some((x.bound, x.scrutinee, x.cases))

    extension (self: MatchType):
      def bound: Type = reflectSelf.MatchType_bound(self)
      def scrutinee: Type = reflectSelf.MatchType_scrutinee(self)
      def cases: List[Type] = reflectSelf.MatchType_cases(self)
    end extension
  end MatchType


  /**
   * An accessor for `scala.internal.MatchCase[_,_]`, the representation of a `MatchType` case.
   */
  def MatchCaseType: Type = {
    import scala.internal.MatchCase
    Type(classOf[MatchCase[_,_]])
  }

  given TypeTest[Type, ByNameType] = reflectSelf.ByNameType_TypeTest
  given ByNameTypeOps as ByNameType.type = ByNameType

  object ByNameType:
    def apply(underlying: Type): Type = reflectSelf.ByNameType_apply(underlying)
    def unapply(x: ByNameType): Option[Type] = Some(x.underlying)

    extension (self: ByNameType):
      def underlying: Type = reflectSelf.ByNameType_underlying(self)
    end extension
  end ByNameType


  given TypeTest[Type, ParamRef] = reflectSelf.ParamRef_TypeTest
  given ParamRefOps as ParamRef.type = ParamRef

  object ParamRef:
    def unapply(x: ParamRef): Option[(LambdaType, Int)] =
      Some((x.binder, x.paramNum))

    extension (self: ParamRef):
      def binder: LambdaType = reflectSelf.ParamRef_binder(self)
      def paramNum: Int = reflectSelf.ParamRef_paramNum(self)
    end extension
  end ParamRef


  given TypeTest[Type, ThisType] = reflectSelf.ThisType_TypeTest
  given ThisTypeOps as ThisType.type = ThisType

  object ThisType:
    def unapply(x: ThisType): Option[Type] = Some(x.tref)

    extension (self: ThisType):
      def tref: Type = reflectSelf.ThisType_tref(self)
    end extension
  end ThisType


  given TypeTest[Type, RecursiveThis] = reflectSelf.RecursiveThis_TypeTest
  given RecursiveThisOps as RecursiveThis.type = RecursiveThis

  object RecursiveThis:
    def unapply(x: RecursiveThis): Option[RecursiveType] = Some(x.binder)

    extension (self: RecursiveThis):
      def binder: RecursiveType = reflectSelf.RecursiveThis_binder(self)
    end extension
  end RecursiveThis


  given TypeTest[Type, RecursiveType] = reflectSelf.RecursiveType_TypeTest
  given RecursiveTypeOps as RecursiveType.type = RecursiveType

  object RecursiveType:

    /** Create a RecType, normalizing its contents. This means:
     *
     *   1. Nested Rec types on the type's spine are merged with the outer one.
     *   2. Any refinement of the form `type T = z.T` on the spine of the type
     *      where `z` refers to the created rec-type is replaced by
     *      `type T`. This avoids infinite recursions later when we
     *      try to follow these references.
     */
    def apply(parentExp: RecursiveType => Type): RecursiveType =
      reflectSelf.RecursiveType_apply(parentExp)

    def unapply(x: RecursiveType): Option[Type] = Some(x.underlying)

    extension (self: RecursiveType):
      def underlying: Type = reflectSelf.RecursiveType_underlying(self)
      def recThis: RecursiveThis = reflectSelf.RecursiveThis_recThis(self)
    end extension
  end RecursiveType


  given TypeTest[Type, MethodType] = reflectSelf.MethodType_TypeTest
  given MethodTypeOps as MethodType.type = MethodType

  object MethodType:
    def apply(paramNames: List[String])(paramInfosExp: MethodType => List[Type], resultTypeExp: MethodType => Type): MethodType =
      reflectSelf.MethodType_apply(paramNames)(paramInfosExp, resultTypeExp)

    def unapply(x: MethodType): Option[(List[String], List[Type], Type)] =
      Some((x.paramNames, x.paramTypes, x.resType))

    extension (self: MethodType):
      def isImplicit: Boolean = reflectSelf.MethodType_isImplicit(self)
      def isErased: Boolean = reflectSelf.MethodType_isErased(self)
      def param(idx: Int): Type = reflectSelf.MethodType_param(self, idx)
      def paramNames: List[String] = reflectSelf.MethodType_paramNames(self)
      def paramTypes: List[Type] = reflectSelf.MethodType_paramTypes(self)
      def resType: Type = reflectSelf.MethodType_resType(self)
    end extension
  end MethodType


  given TypeTest[Type, PolyType] = reflectSelf.PolyType_TypeTest
  given PolyTypeOps as PolyType.type = PolyType

  object PolyType:
    def apply(paramNames: List[String])(paramBoundsExp: PolyType => List[TypeBounds], resultTypeExp: PolyType => Type): PolyType =
      reflectSelf.PolyType_apply(paramNames)(paramBoundsExp, resultTypeExp)
    def unapply(x: PolyType): Option[(List[String], List[TypeBounds], Type)] =
      Some((x.paramNames, x.paramBounds, x.resType))

    extension (self: PolyType):
      def param(idx: Int): Type = reflectSelf.PolyType_param(self, idx)
      def paramNames: List[String] = reflectSelf.PolyType_paramNames(self)
      def paramBounds: List[TypeBounds] = reflectSelf.PolyType_paramBounds(self)
      def resType: Type = reflectSelf.PolyType_resType(self)
    end extension
  end PolyType


  given TypeTest[Type, TypeLambda] = reflectSelf.TypeLambda_TypeTest
  given TypeLambdaOps as TypeLambda.type = TypeLambda

  object TypeLambda:
    def apply(paramNames: List[String], boundsFn: TypeLambda => List[TypeBounds], bodyFn: TypeLambda => Type): TypeLambda =
      reflectSelf.TypeLambda_apply(paramNames, boundsFn, bodyFn)
    def unapply(x: TypeLambda): Option[(List[String], List[TypeBounds], Type)] =
      Some((x.paramNames, x.paramBounds, x.resType))

    extension (self: TypeLambda):
      def paramNames: List[String] = reflectSelf.TypeLambda_paramNames(self)
      def paramBounds: List[TypeBounds] = reflectSelf.TypeLambda_paramBounds(self)
      def param(idx: Int) : Type = reflectSelf.TypeLambda_param(self, idx)
      def resType: Type = reflectSelf.TypeLambda_resType(self)
    end extension
  end TypeLambda


  // ----- TypeBounds -----------------------------------------------

  given TypeTest[Type, TypeBounds] = reflectSelf.TypeBounds_TypeTest
  given TypeBoundsOps as TypeBounds.type = TypeBounds

  object TypeBounds:
    def apply(low: Type, hi: Type): TypeBounds =
      reflectSelf.TypeBounds_apply(low, hi)
    def unapply(x: TypeBounds): Option[(Type, Type)] = Some((x.low, x.hi))

    extension (self: TypeBounds):
      def low: Type = reflectSelf.TypeBounds_low(self)
      def hi: Type = reflectSelf.TypeBounds_hi(self)
    end extension
  end TypeBounds


  // ----- NoPrefix -------------------------------------------------

  given TypeTest[Type, NoPrefix] = reflectSelf.NoPrefix_TypeTest

  object NoPrefix:
    def unapply(x: NoPrefix): Boolean = true


  ///////////////
  // CONSTANTS //
  ///////////////

  given ConstantOps as Constant.type = Constant

  /** Module of Constant literals */
  object Constant:

    def apply(x: Unit | Null | Int | Boolean | Byte | Short | Int | Long | Float | Double | Char | String | Type): Constant =
      reflectSelf.Constant_apply(x)

    def unapply(constant: Constant): Option[Unit | Null | Int | Boolean | Byte | Short | Int | Long | Float | Double | Char | String | Type] =
      reflectSelf.matchConstant(constant)

    /** Module of ClassTag literals */
    object ClassTag:
      /** scala.reflect.ClassTag literal */
      def apply[T](using x: Type): Constant =
        reflectSelf.Constant_ClassTag_apply(x)

      /** Extractor for ClassTag literals */
      def unapply(constant: Constant): Option[Type] =
        reflectSelf.matchConstant_ClassTag(constant)
    end ClassTag

    extension (const: Constant):
      /** Returns the value of the constant */
      def value: Any = reflectSelf.Constant_value(const)

      /** Shows the tree as extractors */
      def showExtractors: String =
        new ExtractorsPrinter[reflectSelf.type](reflectSelf).showConstant(const)

      /** Shows the tree as fully typed source code */
      def show: String =
        const.showWith(SyntaxHighlight.plain)

      /** Shows the tree as fully typed source code */
      def showWith(syntaxHighlight: SyntaxHighlight): String =
        new SourceCodePrinter[reflectSelf.type](reflectSelf)(syntaxHighlight).showConstant(const)
    end extension
  end Constant


  /////////
  // IDs //
  /////////

  given IdOps as Id.type = Id

  object Id:
    def unapply(id: Id): Option[String] = Some(id.name)

    extension (id: Id):
      /** Position in the source code */
      def pos: Position = reflectSelf.Id_pos(id)

      /** Name of the identifier */
      def name: String = reflectSelf.Id_name(id)
    end extension
  end Id


  /////////////////////
  // IMPLICIT SEARCH //
  /////////////////////

  def searchImplicit(tpe: Type): ImplicitSearchResult

  given TypeTest[ImplicitSearchResult, ImplicitSearchSuccess] = reflectSelf.ImplicitSearchSuccess_TypeTest
  given ImplicitSearchSuccessOps as ImplicitSearchSuccess.type = ImplicitSearchSuccess

  object ImplicitSearchSuccess:
    extension (self: ImplicitSearchSuccess):
      def tree: Term = reflectSelf.ImplicitSearchSuccess_tree(self)
    end extension
  end ImplicitSearchSuccess

  given TypeTest[ImplicitSearchResult, ImplicitSearchFailure] = reflectSelf.ImplicitSearchFailure_TypeTest
  given ImplicitSearchFailureOps as ImplicitSearchFailure.type = ImplicitSearchFailure

  object ImplicitSearchFailure:
    extension (self: ImplicitSearchFailure):
      def explanation: String = reflectSelf.ImplicitSearchFailure_explanation(self)
    end extension
  end ImplicitSearchFailure

  given TypeTest[ImplicitSearchResult, DivergingImplicit] = reflectSelf.DivergingImplicit_TypeTest

  given TypeTest[ImplicitSearchResult, NoMatchingImplicits] = reflectSelf.NoMatchingImplicits_TypeTest

  given TypeTest[ImplicitSearchResult, AmbiguousImplicits] = reflectSelf.AmbiguousImplicits_TypeTest


  /////////////
  // SYMBOLS //
  /////////////

  given SymbolOps as Symbol.type = Symbol

  object Symbol:

    /** Get package symbol if package is either defined in current compilation run or present on classpath. */
    def requiredPackage(path: String): Symbol = reflectSelf.Symbol_requiredPackage(path)

    /** Get class symbol if class is either defined in current compilation run or present on classpath. */
    def requiredClass(path: String): Symbol = reflectSelf.Symbol_requiredClass(path)

    /** Get module symbol if module is either defined in current compilation run or present on classpath. */
    def requiredModule(path: String): Symbol = reflectSelf.Symbol_requiredModule(path)

    /** Get method symbol if method is either defined in current compilation run or present on classpath. Throws if the method has an overload. */
    def requiredMethod(path: String): Symbol = reflectSelf.Symbol_requiredMethod(path)

    /** The class Symbol of a global class definition */
    def classSymbol(fullName: String): Symbol =
      reflectSelf.Symbol_of(fullName)

    /** Generates a new method symbol with the given parent, name and type.
     *
     *  This symbol starts without an accompanying definition.
     *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
     *  this symbol to the DefDef constructor.
     *
     *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
     *        direct or indirect children of the reflection context's owner.
     */
    def newMethod(parent: Symbol, name: String, tpe: Type): Symbol =
      newMethod(parent, name, tpe, Flags.EmptyFlags, noSymbol)

    /** Works as the other newMethod, but with additional parameters.
     *
     *  @param flags extra flags to with which the symbol should be constructed
     *  @param privateWithin the symbol within which this new method symbol should be private. May be noSymbol.
     */
    def newMethod(parent: Symbol, name: String, tpe: Type, flags: Flags, privateWithin: Symbol): Symbol =
      reflectSelf.Symbol_newMethod(parent, name, flags, tpe, privateWithin)

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
    def newVal(parent: Symbol, name: String, tpe: Type, flags: Flags, privateWithin: Symbol): Symbol =
      reflectSelf.Symbol_newVal(parent, name, flags, tpe, privateWithin)

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
    def newBind(parent: Symbol, name: String, flags: Flags, tpe: Type): Symbol =
      reflectSelf.Symbol_newBind(parent, name, flags, tpe)

    /** Definition not available */
    def noSymbol: Symbol =
      reflectSelf.Symbol_noSymbol

    extension (sym: Symbol):

      /** Owner of this symbol. The owner is the symbol in which this symbol is defined. Throws if this symbol does not have an owner. */
      def owner: Symbol = reflectSelf.Symbol_owner(sym)

      /** Owner of this symbol. The owner is the symbol in which this symbol is defined. Returns `NoSymbol` if this symbol does not have an owner. */
      def maybeOwner: Symbol = reflectSelf.Symbol_maybeOwner(sym)

      /** Flags of this symbol */
      def flags: Flags = reflectSelf.Symbol_flags(sym)

      /** This symbol is private within the resulting type */
      def privateWithin: Option[Type] = reflectSelf.Symbol_privateWithin(sym)

      /** This symbol is protected within the resulting type */
      def protectedWithin: Option[Type] = reflectSelf.Symbol_protectedWithin(sym)

      /** The name of this symbol */
      def name: String = reflectSelf.Symbol_name(sym)

      /** The full name of this symbol up to the root package */
      def fullName: String = reflectSelf.Symbol_fullName(sym)

      /** The position of this symbol */
      def pos: Position = reflectSelf.Symbol_pos(sym)

      /** The comment for this symbol, if any */
      def comment: Option[Comment] = reflectSelf.Symbol_comment(sym)

      /** Tree of this definition
        *
        * if this symbol `isPackageDef` it will return a `PackageDef`,
        * if this symbol `isClassDef` it will return a `ClassDef`,
        * if this symbol `isTypeDef` it will return a `TypeDef`,
        * if this symbol `isValDef` it will return a `ValDef`,
        * if this symbol `isDefDef` it will return a `DefDef`
        * if this symbol `isBind` it will return a `Bind`
        */
      def tree: Tree =
        reflectSelf.Symbol_tree(sym)

      /** Annotations attached to this symbol */
      def annots: List[Term] = reflectSelf.Symbol_annots(sym)

      def isDefinedInCurrentRun: Boolean = reflectSelf.Symbol_isDefinedInCurrentRun(sym)

      def isLocalDummy: Boolean = reflectSelf.Symbol_isLocalDummy(sym)
      def isRefinementClass: Boolean = reflectSelf.Symbol_isRefinementClass(sym)
      def isAliasType: Boolean = reflectSelf.Symbol_isAliasType(sym)
      def isAnonymousClass: Boolean = reflectSelf.Symbol_isAnonymousClass(sym)
      def isAnonymousFunction: Boolean = reflectSelf.Symbol_isAnonymousFunction(sym)
      def isAbstractType: Boolean = reflectSelf.Symbol_isAbstractType(sym)
      def isClassConstructor: Boolean = reflectSelf.Symbol_isClassConstructor(sym)

      /** Is this the definition of a type? */
      def isType: Boolean = reflectSelf.Symbol_isType(sym)

      /** Is this the definition of a term? */
      def isTerm: Boolean = reflectSelf.Symbol_isTerm(sym)

      /** Is this the definition of a PackageDef tree? */
      def isPackageDef: Boolean = reflectSelf.Symbol_isPackageDef(sym)

      /** Is this the definition of a ClassDef tree? */
      def isClassDef: Boolean = reflectSelf.Symbol_isClassDef(sym)

      /** Is this the definition of a TypeDef tree */
      def isTypeDef: Boolean = reflectSelf.Symbol_isTypeDef(sym)

      /** Is this the definition of a ValDef tree? */
      def isValDef: Boolean = reflectSelf.Symbol_isValDef(sym)

      /** Is this the definition of a DefDef tree? */
      def isDefDef: Boolean = reflectSelf.Symbol_isDefDef(sym)

      /** Is this the definition of a Bind pattern? */
      def isBind: Boolean = reflectSelf.Symbol_isBind(sym)

      /** Does this symbol represent a no definition? */
      def isNoSymbol: Boolean = sym == Symbol.noSymbol

      /** Does this symbol represent a definition? */
      def exists: Boolean = sym != Symbol.noSymbol

      /** Fields directly declared in the class */
      def fields: List[Symbol] =
        reflectSelf.Symbol_fields(sym)

      /** Field with the given name directly declared in the class */
      def field(name: String): Symbol =
        reflectSelf.Symbol_field(sym)(name)

      /** Get non-private named methods defined directly inside the class */
      def classMethod(name: String): List[Symbol] =
        reflectSelf.Symbol_classMethod(sym)(name)

      /** Get all non-private methods defined directly inside the class, exluding constructors */
      def classMethods: List[Symbol] =
        reflectSelf.Symbol_classMethods(sym)

      /** Type member directly declared in the class */
      def typeMembers: List[Symbol] =
        reflectSelf.Symbol_typeMembers(sym)

      /** Type member with the given name directly declared in the class */
      def typeMember(name: String): Symbol =
        reflectSelf.Symbol_typeMember(sym)(name)

      /** Get named non-private methods declared or inherited */
      def method(name: String): List[Symbol] =
        reflectSelf.Symbol_method(sym)(name)

      /** Get all non-private methods declared or inherited */
      def methods: List[Symbol] =
        reflectSelf.Symbol_methods(sym)

      /** The symbols of each type parameter list and value parameter list of this
        *  method, or Nil if this isn't a method.
        */
      def paramSymss: List[List[Symbol]] =
        reflectSelf.Symbol_paramSymss(sym)

      /** The primary constructor of a class or trait, `noSymbol` if not applicable. */
      def primaryConstructor: Symbol =
        reflectSelf.Symbol_primaryConstructor(sym)

      /** Fields of a case class type -- only the ones declared in primary constructor */
      def caseFields: List[Symbol] =
        reflectSelf.Symbol_caseFields(sym)

      def isTypeParam: Boolean =
        reflectSelf.Symbol_isTypeParam(sym)

      /** Signature of this definition */
      def signature: Signature =
        reflectSelf.Symbol_signature(sym)

      /** The class symbol of the companion module class */
      def moduleClass: Symbol =
        reflectSelf.Symbol_moduleClass(sym)

      /** The symbol of the companion class */
      def companionClass: Symbol =
        reflectSelf.Symbol_companionClass(sym)

      /** The symbol of the companion module */
      def companionModule: Symbol =
        reflectSelf.Symbol_companionModule(sym)

      /** Shows the tree as extractors */
      def showExtractors: String =
        new ExtractorsPrinter[reflectSelf.type](reflectSelf).showSymbol(sym)

      /** Shows the tree as fully typed source code */
      def show: String =
        sym.showWith(SyntaxHighlight.plain)

      /** Shows the tree as fully typed source code */
      def showWith(syntaxHighlight: SyntaxHighlight): String =
        new SourceCodePrinter[reflectSelf.type](reflectSelf)(syntaxHighlight).showSymbol(sym)

      /** Case class or case object children of a sealed trait */
      def children: List[Symbol] =
        reflectSelf.Symbol_children(sym)
    end extension
  end Symbol




  ////////////////
  // SIGNATURES //
  ////////////////

  given SignatureOps as Signature.type = Signature

  /** The signature of a method */
  object Signature:
    /** Matches the method signature and returns its parameters and result type. */
    def unapply(sig: Signature): Option[(List[String | Int], String)] =
      Some((sig.paramSigs, sig.resultSig))

    extension (sig: Signature):

      /** The signatures of the method parameters.
        *
        *  Each *type parameter section* is represented by a single Int corresponding
        *  to the number of type parameters in the section.
        *  Each *term parameter* is represented by a String corresponding to the fully qualified
        *  name of the parameter type.
        */
      def paramSigs: List[String | Int] = reflectSelf.Signature_paramSigs(sig)

      /** The signature of the result type */
      def resultSig: String = reflectSelf.Signature_resultSig(sig)

    end extension
  end Signature


  //////////////////////////
  // STANDARD DEFINITIONS //
  //////////////////////////

  /** A value containing all standard definitions in [[DefinitionsAPI]]
   *  @group Definitions
   */
  object defn extends StandardSymbols with StandardTypes

  /** Defines standard symbols (and types via its base trait).
   *  @group API
   */
  trait StandardSymbols {

    /** The module symbol of root package `_root_`. */
    def RootPackage: Symbol = reflectSelf.Definitions_RootPackage

    /** The class symbol of root package `_root_`. */
    def RootClass: Symbol = reflectSelf.Definitions_RootClass

    /** The class symbol of empty package `_root_._empty_`. */
    def EmptyPackageClass: Symbol = reflectSelf.Definitions_EmptyPackageClass

    /** The module symbol of package `scala`. */
    def ScalaPackage: Symbol = reflectSelf.Definitions_ScalaPackage

    /** The class symbol of package `scala`. */
    def ScalaPackageClass: Symbol = reflectSelf.Definitions_ScalaPackageClass

    /** The class symbol of core class `scala.Any`. */
    def AnyClass: Symbol = reflectSelf.Definitions_AnyClass

    /** The class symbol of core class `scala.AnyVal`. */
    def AnyValClass: Symbol = reflectSelf.Definitions_AnyValClass

    /** The class symbol of core class `java.lang.Object`. */
    def ObjectClass: Symbol = reflectSelf.Definitions_ObjectClass

    /** The type symbol of core class `scala.AnyRef`. */
    def AnyRefClass: Symbol = reflectSelf.Definitions_AnyRefClass

    /** The class symbol of core class `scala.Null`. */
    def NullClass: Symbol = reflectSelf.Definitions_NullClass

    /** The class symbol of core class `scala.Nothing`. */
    def NothingClass: Symbol = reflectSelf.Definitions_NothingClass

    /** The class symbol of primitive class `scala.Unit`. */
    def UnitClass: Symbol = reflectSelf.Definitions_UnitClass

    /** The class symbol of primitive class `scala.Byte`. */
    def ByteClass: Symbol = reflectSelf.Definitions_ByteClass

    /** The class symbol of primitive class `scala.Short`. */
    def ShortClass: Symbol = reflectSelf.Definitions_ShortClass

    /** The class symbol of primitive class `scala.Char`. */
    def CharClass: Symbol = reflectSelf.Definitions_CharClass

    /** The class symbol of primitive class `scala.Int`. */
    def IntClass: Symbol = reflectSelf.Definitions_IntClass

    /** The class symbol of primitive class `scala.Long`. */
    def LongClass: Symbol = reflectSelf.Definitions_LongClass

    /** The class symbol of primitive class `scala.Float`. */
    def FloatClass: Symbol = reflectSelf.Definitions_FloatClass

    /** The class symbol of primitive class `scala.Double`. */
    def DoubleClass: Symbol = reflectSelf.Definitions_DoubleClass

    /** The class symbol of primitive class `scala.Boolean`. */
    def BooleanClass: Symbol = reflectSelf.Definitions_BooleanClass

    /** The class symbol of class `scala.String`. */
    def StringClass: Symbol = reflectSelf.Definitions_StringClass

    /** The class symbol of class `java.lang.Class`. */
    def ClassClass: Symbol = reflectSelf.Definitions_ClassClass

    /** The class symbol of class `scala.Array`. */
    def ArrayClass: Symbol = reflectSelf.Definitions_ArrayClass

    /** The module symbol of module `scala.Predef`. */
    def PredefModule: Symbol = reflectSelf.Definitions_PredefModule

    /** The method symbol of method `scala.Predef.classOf`. */
    def Predef_classOf: Symbol = reflectSelf.Definitions_Predef_classOf

    /** The module symbol of package `java.lang`. */
    def JavaLangPackage: Symbol = reflectSelf.Definitions_JavaLangPackage

    /** The module symbol of module `scala.Array`. */
    def ArrayModule: Symbol = reflectSelf.Definitions_ArrayModule

    /** The method symbol of method `apply` in class `scala.Array`. */
    def Array_apply: Symbol = reflectSelf.Definitions_Array_apply

    /** The method symbol of method `clone` in class `scala.Array`. */
    def Array_clone: Symbol = reflectSelf.Definitions_Array_clone

    /** The method symbol of method `length` in class `scala.Array`. */
    def Array_length: Symbol = reflectSelf.Definitions_Array_length

    /** The method symbol of method `update` in class `scala.Array`. */
    def Array_update: Symbol = reflectSelf.Definitions_Array_update

    /** A dummy class symbol that is used to indicate repeated parameters
     *  compiled by the Scala compiler.
     */
    def RepeatedParamClass: Symbol = reflectSelf.Definitions_RepeatedParamClass

    /** The class symbol of class `scala.annotation.reflectSelf.Repeated` */
    def RepeatedAnnot: Symbol = reflectSelf.Definitions_RepeatedAnnot

    /** The class symbol of class `scala.Option`. */
    def OptionClass: Symbol = reflectSelf.Definitions_OptionClass

    /** The module symbol of module `scala.None`. */
    def NoneModule: Symbol = reflectSelf.Definitions_NoneModule

    /** The module symbol of module `scala.Some`. */
    def SomeModule: Symbol = reflectSelf.Definitions_SomeModule

    /** Function-like object that maps arity to symbols for classes `scala.Product` */
    def ProductClass: Symbol = reflectSelf.Definitions_ProductClass

    /** Function-like object that maps arity to symbols for classes `scala.FunctionX`.
     *   -  0th element is `Function0`
     *   -  1st element is `Function1`
     *   -  ...
     *   -  Nth element is `FunctionN`
     */
    def FunctionClass(arity: Int, isImplicit: Boolean = false, isErased: Boolean = false): Symbol =
      reflectSelf.Definitions_FunctionClass(arity, isImplicit, isErased)

    /** Function-like object that maps arity to symbols for classes `scala.TupleX`.
     *   -  0th element is `NoSymbol`
     *   -  1st element is `NoSymbol`
     *   -  2st element is `Tuple2`
     *   -  ...
     *   - 22nd element is `Tuple22`
     *   - 23nd element is `NoSymbol`  // TODO update when we will have more tuples
     *   - ...
     */
    def TupleClass(arity: Int): Symbol =
      reflectSelf.Definitions_TupleClass(arity)

    /** Returns `true` if `sym` is a `Tuple1`, `Tuple2`, ... `Tuple22` */
    def isTupleClass(sym: Symbol): Boolean =
      reflectSelf.Definitions_isTupleClass(sym)

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
    def ScalaPrimitiveValueClasses: List[Symbol] =
      UnitClass :: BooleanClass :: ScalaNumericValueClasses

    /** Contains Scala numeric value classes:
     *   - Byte
     *   - Short
     *   - Int
     *   - Long
     *   - Float
     *   - Double
     *   - Char
     */
    def ScalaNumericValueClasses: List[Symbol] =
      ByteClass :: ShortClass :: IntClass :: LongClass :: FloatClass :: DoubleClass :: CharClass :: Nil

  }

  /** Defines standard types.
   *  @group Definitions
   */
  trait StandardTypes {
    /** The type of primitive type `Unit`. */
    def UnitType: Type = reflectSelf.Definitions_UnitType

    /** The type of primitive type `Byte`. */
    def ByteType: Type = reflectSelf.Definitions_ByteType

    /** The type of primitive type `Short`. */
    def ShortType: Type = reflectSelf.Definitions_ShortType

    /** The type of primitive type `Char`. */
    def CharType: Type = reflectSelf.Definitions_CharType

    /** The type of primitive type `Int`. */
    def IntType: Type = reflectSelf.Definitions_IntType

    /** The type of primitive type `Long`. */
    def LongType: Type = reflectSelf.Definitions_LongType

    /** The type of primitive type `Float`. */
    def FloatType: Type = reflectSelf.Definitions_FloatType

    /** The type of primitive type `Double`. */
    def DoubleType: Type = reflectSelf.Definitions_DoubleType

    /** The type of primitive type `Boolean`. */
    def BooleanType: Type = reflectSelf.Definitions_BooleanType

    /** The type of core type `Any`. */
    def AnyType: Type = reflectSelf.Definitions_AnyType

    /** The type of core type `AnyVal`. */
    def AnyValType: Type = reflectSelf.Definitions_AnyValType

    /** The type of core type `AnyRef`. */
    def AnyRefType: Type = reflectSelf.Definitions_AnyRefType

    /** The type of core type `Object`. */
    def ObjectType: Type = reflectSelf.Definitions_ObjectType

    /** The type of core type `Nothing`. */
    def NothingType: Type = reflectSelf.Definitions_NothingType

    /** The type of core type `Null`. */
    def NullType: Type = reflectSelf.Definitions_NullType

    /** The type for `scala.String`. */
    def StringType: Type = reflectSelf.Definitions_StringType

    /** The type for `scala.Tuple`. */
    def TupleType: Type = reflectSelf.Definitions_TupleType

    /** The type for `scala.EmptyTuple`. */
    def EmptyTupleType: Type = reflectSelf.Definitions_EmptyTupleType

    /** The type for `scala.NonEmptyTuple`. */
    def NonEmptyTupleType: Type = reflectSelf.Definitions_NonEmptyTupleType

    /** The type for `scala.*:`. */
    def TupleConsType: Type = reflectSelf.Definitions_TupleConsType
  }


  ///////////////
  //   FLAGS   //
  ///////////////

  given FlagsOps as Flags.type = Flags

  object Flags:

    /** Is this symbol `abstract` */
    def Abstract: Flags = reflectSelf.Flags_Abstract

    /** Was this symbol generated by Scala compiler */
    def Artifact: Flags = reflectSelf.Flags_Artifact

    /** Is this symbol `case` */
    def Case: Flags = reflectSelf.Flags_Case

    /** Is this symbol a getter for case class parameter */
    def CaseAcessor: Flags = reflectSelf.Flags_CaseAcessor

    /** Is this symbol a type parameter marked as contravariant `-` */
    def Contravariant: Flags = reflectSelf.Flags_Contravariant

    /** Is this symbol a type parameter marked as covariant `+` */
    def Covariant: Flags = reflectSelf.Flags_Covariant

    /** The empty set of flags */
    def EmptyFlags = reflectSelf.Flags_EmptyFlags

    /** Is this symbol an enum */
    def Enum: Flags = reflectSelf.Flags_Enum

    /** Is this symbol `erased` */
    def Erased: Flags = reflectSelf.Flags_Erased

    /** Is this symbol a `def` defined in an `extension` */
    def ExtensionMethod: Flags = reflectSelf.Flags_ExtensionMethod

    /** Is this symbol a getter or a setter */
    def FieldAccessor: Flags = reflectSelf.Flags_FieldAccessor

    /** Is this symbol `final` */
    def Final: Flags = reflectSelf.Flags_Final

    /** Is this symbol an inferable ("given") parameter */
    def Given: Flags = reflectSelf.Flags_Given

    /** Is this symbol a parameter with a default value? */
    def HasDefault: Flags = reflectSelf.Flags_HasDefault

    /** Is this symbol `implicit` */
    def Implicit: Flags = reflectSelf.Flags_Implicit

    /** Is this symbol `inline` */
    def Inline: Flags = reflectSelf.Flags_Inline

    /** Is this symbol defined in a Java class */
    def JavaDefined: Flags = reflectSelf.Flags_JavaDefined

    /** Is this symbol `lazy` */
    def Lazy: Flags = reflectSelf.Flags_Lazy

    /** Is this symbol local? Used in conjunction with private/private[Type] to mean private[this] extends Modifier proctected[this] */
    def Local: Flags = reflectSelf.Flags_Local

    /** Is this symbol marked as a macro. An inline method containing toplevel splices */
    def Macro: Flags = reflectSelf.Flags_Macro

    /** Is this symbol a module class */
    def ModuleClass: Flags = reflectSelf.Flags_ModuleClass

    /** Is this symbol a `var` (when used on a ValDef) */
    def Mutable: Flags = reflectSelf.Flags_Mutable

    /** Is this symbol an object or its class (used for a ValDef or a ClassDef extends Modifier respectively) */
    def Object: Flags = reflectSelf.Flags_Object

    /** Is this symbol `override` */
    def Override: Flags = reflectSelf.Flags_Override

    /** Is this symbol a package */
    def Package: Flags = reflectSelf.Flags_Package

    /** Is this symbol a parameter */
    def Param: Flags = reflectSelf.Flags_Param

    /** Is this symbol a parameter accessor */
    def ParamAccessor: Flags = reflectSelf.Flags_ParamAccessor

    /** Is this symbol `private` */
    def Private: Flags = reflectSelf.Flags_Private

    /** Is this symbol labeled private[this] */
    def PrivateLocal: Flags = reflectSelf.Flags_PrivateLocal

    /** Is this symbol `protected` */
    def Protected: Flags = reflectSelf.Flags_Protected

    /** Was this symbol imported from Scala2.x */
    def Scala2X: Flags = reflectSelf.Flags_Scala2X

    /** Is this symbol `sealed` */
    def Sealed: Flags = reflectSelf.Flags_Sealed

    /** Is this symbol member that is assumed to be stable and realizable */
    def StableRealizable: Flags = reflectSelf.Flags_StableRealizable

    /** Is this symbol marked as static. Mapped to static Java member */
    def Static: Flags = reflectSelf.Flags_Static

    /** Is this symbol to be tagged Java Synthetic */
    def Synthetic: Flags = reflectSelf.Flags_Synthetic

    /** Is this symbol a trait */
    def Trait: Flags = reflectSelf.Flags_Trait

    extension (flags: Flags):
      /** Is the given flag set a subset of this flag sets */
      def is(that: Flags): Boolean = reflectSelf.Flags_is(flags)(that)

      /** Union of the two flag sets */
      def |(that: Flags): Flags = reflectSelf.Flags_or(flags)(that)

      /** Intersection of the two flag sets */
      def &(that: Flags): Flags = reflectSelf.Flags_and(flags)(that)

      /** Shows the tree as extractors */
      def showExtractors: String =
        new ExtractorsPrinter[reflectSelf.type](reflectSelf).showFlags(flags)

      /** Shows the tree as fully typed source code */
      def show: String =
        flags.showWith(SyntaxHighlight.plain)

      /** Shows the tree as fully typed source code */
      def showWith(syntaxHighlight: SyntaxHighlight): String =
        new SourceCodePrinter[reflectSelf.type](reflectSelf)(syntaxHighlight).showFlags(flags)

    end extension
  end Flags


  ///////////////
  // POSITIONS //
  ///////////////

  // TODO: Should this be in the QuoteContext?
  // TODO: rename to enclosingPosition (as in scala.reflect)
  /** Root position of this tasty context. For macros it corresponds to the expansion site. */
  def rootPosition: Position

  given PositionOps as Position.type = Position
  object Position:
    extension (pos: Position):

      /** The start offset in the source file */
      def start: Int = reflectSelf.Position_start(pos)

      /** The end offset in the source file */
      def end: Int = reflectSelf.Position_end(pos)

      /** Does this position exist */
      def exists: Boolean = reflectSelf.Position_exists(pos)

      /** Source file in which this position is located */
      def sourceFile: SourceFile = reflectSelf.Position_sourceFile(pos)

      /** The start line in the source file */
      def startLine: Int = reflectSelf.Position_startLine(pos)

      /** The end line in the source file */
      def endLine: Int = reflectSelf.Position_endLine(pos)

      /** The start column in the source file */
      def startColumn: Int = reflectSelf.Position_startColumn(pos)

      /** The end column in the source file */
      def endColumn: Int = reflectSelf.Position_endColumn(pos)

      /** Source code within the position */
      def sourceCode: String = reflectSelf.Position_sourceCode(pos)

    end extension
  end Position

  given SourceFileOps as SourceFile.type = SourceFile
  object SourceFile:
    extension (sourceFile: SourceFile):

      /** Path to this source file */
      def jpath: java.nio.file.Path = reflectSelf.SourceFile_jpath(sourceFile)

      /** Content of this source file */
      def content: String = reflectSelf.SourceFile_content(sourceFile)

    end extension
  end SourceFile

  ///////////////
  // REPORTING //
  ///////////////

  /** Emits an error message */
  def error(msg: => String, pos: Position): Unit

  /** Emits an error at a specific range of a file */
  def error(msg: => String, source: SourceFile, start: Int, end: Int): Unit

  /** Emits an error message */
  def warning(msg: => String, pos: Position): Unit

  /** Emits a warning at a specific range of a file */
  def warning(msg: => String, source: SourceFile, start: Int, end: Int): Unit


  //////////////
  // COMMENTS //
  //////////////

  given CommentOps as Comment.type = Comment

  object Comment:
    extension (self: Comment):

      /** Raw comment string */
      def raw: String = reflectSelf.Comment_raw(self)

      /** Expanded comment string, if any */
      def expanded: Option[String] = reflectSelf.Comment_expanded(self)

      /** List of usecases and their corresponding trees, if any */
      def usecases: List[(String, Option[DefDef])] = reflectSelf.Comment_usecases(self)

    end extension
  end Comment


  ///////////////
  //   UTILS   //
  ///////////////

  /** TASTy Reflect tree accumulator */
  trait TreeAccumulator[X] extends reflect.TreeAccumulator[X] {
    val reflect: reflectSelf.type = reflectSelf
  }

  /** TASTy Reflect tree traverser */
  trait TreeTraverser extends reflect.TreeTraverser {
    val reflect: reflectSelf.type = reflectSelf
  }

  /** TASTy Reflect tree map */
  trait TreeMap extends reflect.TreeMap {
    val reflect: reflectSelf.type = reflectSelf
  }

  // TODO extract from Reflection

  /** Bind the `rhs` to a `val` and use it in `body` */
  def let(rhs: Term)(body: Ident => Term): Term = {
    val sym = Symbol.newVal(Owner.current.symbol, "x", rhs.tpe.widen, Flags.EmptyFlags, Symbol.noSymbol)
    Block(List(ValDef(sym, Some(rhs))), body(Ref(sym).asInstanceOf[Ident]))
  }

  /** Bind the given `terms` to names and use them in the `body` */
  def lets(terms: List[Term])(body: List[Term] => Term): Term = {
    def rec(xs: List[Term], acc: List[Term]): Term = xs match {
      case Nil => body(acc)
      case x :: xs => let(x) { (x: Term) => rec(xs, x :: acc) }
    }
    rec(terms, Nil)
  }

}
