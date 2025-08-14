package scala.quoted.meta

import scala.annotation.targetName
import scala.quoted.{Expr, Type}

sealed trait Tree {
  type Self <: Tree

  /** Position in the source code */
  def pos: Position

  /** Symbol of defined or referred by this tree */
  def symbol: Symbol

  /** Shows the tree as String */
  def show(using Printer[Tree]): String

  /** Does this tree represent a valid expression? */
  def isExpr: Boolean

  /** Convert this tree to an `quoted.Expr[Any]` if the tree is a valid expression or throws */
  def asExpr: Expr[Any]

  def asExprOf[T](using Type[T]): Expr[T]

  def changeOwner(newOwner: Symbol): Self

}
object Tree {

  def api(using meta: Meta): Meta.TreeAPI = meta.internal.tree
  given Meta => Conversion[Tree.type, Meta.TreeAPI] = _.api

}

trait PackageClause private[meta] extends Tree {
  override type Self <: PackageClause

  /** Tree containing the package name */
  def pid: Ref

  /** Definitions, imports or exports within the package */
  def stats: List[Tree]

}
object PackageClause {

  def api(using meta: Meta): Meta.PackageClauseAPI = meta.internal.packageClause
  given Meta => Conversion[PackageClause.type, Meta.PackageClauseAPI] = _.api

  /** Matches a package clause `package pid { stats }` and extracts the `pid` and `stats` */
  def unapply(tree: PackageClause): (Ref, List[Tree]) = (tree.pid, tree.stats)

}

sealed trait Statement extends Tree {
  override type Self <: Statement
}
object Statement {

  def api(using meta: Meta): Meta.StatementAPI = meta.internal.statement
  given Meta => Conversion[Statement.type, Meta.StatementAPI] = _.api

}

sealed trait ImportOrExport extends Statement {
  override type Self <: ImportOrExport

  /** Qualifier of the import/export */
  def expr: Term

  /**
    * List selectors of the import/export
    *
    *  See documentation on `Selector`
    */
  def selectors: List[Selector]

}
object ImportOrExport {

  def api(using meta: Meta): Meta.ImportOrExportAPI = meta.internal.importOrExport
  given Meta => Conversion[ImportOrExport.type, Meta.ImportOrExportAPI] = _.api

}

trait Import private[meta] extends ImportOrExport {
  override type Self <: Import
}
object Import {

  def api(using meta: Meta): Meta.ImportAPI = meta.internal.`import`
  given Meta => Conversion[Import.type, Meta.ImportAPI] = _.api

  /** Matches an `Import` and extracts the qualifier and selectors */
  def unapply(tree: Import): (Term, List[Selector]) = (tree.expr, tree.selectors)

}

trait Export private[meta] extends ImportOrExport {
  override type Self <: Export
}
object Export {

  def api(using meta: Meta): Meta.ExportAPI = meta.internal.`export`
  given Meta => Conversion[Export.type, Meta.ExportAPI] = _.api

  /** Matches an `Export` and extracts the qualifier and selectors */
  def unapply(tree: Export): (Term, List[Selector]) = (tree.expr, tree.selectors)

}

sealed trait Definition extends Statement {
  override type Self <: Definition

  /** Name of the definition */
  def name: String

}
object Definition {

  def api(using meta: Meta): Meta.DefinitionAPI = meta.internal.definition
  given Meta => Conversion[Definition.type, Meta.DefinitionAPI] = _.api

}

trait ClassDef private[meta] extends Definition {
  override type Self <: ClassDef

  /** The primary constructor of this class */
  def constructor: DefDef

  /**
    * List of extended parent classes or traits.
    *  The first parent is always a class.
    */
  def parents: List[Term | TypeTree]

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

  def api(using meta: Meta): Meta.ClassDefAPI = meta.internal.classDef
  given Meta => Conversion[ClassDef.type, Meta.ClassDefAPI] = _.api

  def unapply(cdef: ClassDef): (String, DefDef, List[Term | TypeTree], Option[ValDef], List[Statement]) = (cdef.name, cdef.constructor, cdef.parents, cdef.self, cdef.body)

}

trait TypeDef private[meta] extends Definition {
  override type Self <: TypeDef

  /** The type bounds on the right-hand side of this `type` definition */
  def rhs: Tree

}
object TypeDef {

  def api(using meta: Meta): Meta.TypeDefAPI = meta.internal.typeDef
  given Meta => Conversion[TypeDef.type, Meta.TypeDefAPI] = _.api

  def unapply(tdef: TypeDef): (String, Tree) = (tdef.name, tdef.rhs)

}

sealed trait ValOrDefDef extends Definition {
  override type Self <: ValOrDefDef

  /** The type tree of this `val` or `def` definition */
  def tpt: TypeTree

  /** The right-hand side of this `val` or `def` definition */
  def rhs: Option[Term]

}
object ValOrDefDef {

  def api(using meta: Meta): Meta.ValOrDefDefAPI = meta.internal.valOrDefDef
  given Meta => Conversion[ValOrDefDef.type, Meta.ValOrDefDefAPI] = _.api

}

trait DefDef private[meta] extends ValOrDefDef {
  override type Self <: DefDef

  /** List of type and term parameter clauses */
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
    *  Return all parameter clauses if there are no leading type parameters.
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

  /** List of term parameter clauses */
  def termParamss: List[TermParamClause]

  /** The tree of the return type of this `def` definition */
  def returnTpt: TypeTree

}
object DefDef {

  def api(using meta: Meta): Meta.DefDefAPI = meta.internal.defDef
  given Meta => Conversion[DefDef.type, Meta.DefDefAPI] = _.api

  def unapply(ddef: DefDef): (String, List[ParamClause], TypeTree, Option[Term]) = (ddef.name, ddef.paramss, ddef.returnTpt, ddef.rhs)

}

trait ValDef private[meta] extends ValOrDefDef {
  override type Self <: ValDef
}
object ValDef {

  def api(using meta: Meta): Meta.ValDefAPI = meta.internal.valDef
  given Meta => Conversion[ValDef.type, Meta.ValDefAPI] = _.api

  def unapply(vdef: ValDef): (String, TypeTree, Option[Term]) = (vdef.name, vdef.tpt, vdef.rhs)

}

sealed trait Term extends Statement {
  override type Self <: Term

  /** TypeRepr of this term */
  def tpe: TypeRepr

  /**
    * Replace Inlined nodes and InlineProxy references to underlying arguments.
    *  The resulting tree is useful for inspection of the value or content of a non-inline argument.
    *
    *  Warning: This tree may contain references that are out of scope and should not be used in the generated code.
    *           This method should only used to port Scala 2 that used to access their outer scope unsoundly.
    */
  def underlyingArgument: Term

  /**
    * Replace Ident nodes references to the underlying tree that defined them.
    *  The resulting tree is useful for inspection of the definition of some bindings.
    *
    *  Warning: This tree may contain references that are out of scope and should not be used in the generated code.
    *           This method should only used to port Scala 2 that used to access their outer scope unsoundly.
    */
  def underlying: Term

  /** Converts a partially applied term into a lambda expression */
  def etaExpand(owner: Symbol): Term

  /** A unary apply node with given argument: `tree(arg)` */
  def appliedTo(arg: Term): Term

  /** An apply node with given arguments: `tree(arg, args0, ..., argsN)` */
  def appliedTo(arg: Term, args: Term*): Term

  /** An apply node with given argument list `tree(args(0), ..., args(args.length - 1))` */
  def appliedToArgs(args: List[Term]): Apply

  /**
    * The current tree applied to given argument lists:
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

}
object Term {

  def api(using meta: Meta): Meta.TermAPI = meta.internal.term
  given Meta => Conversion[Term.type, Meta.TermAPI] = _.api

}

sealed trait Ref extends Term {
  override type Self <: Ref
}
object Ref {

  def api(using meta: Meta): Meta.RefAPI = meta.internal.ref
  given Meta => Conversion[Ref.type, Meta.RefAPI] = _.api

}

trait Ident private[meta] extends Ref {
  override type Self <: Ident

  /** Name of this `Ident` */
  def name: String

}
object Ident {

  def api(using meta: Meta): Meta.IdentAPI = meta.internal.ident
  given Meta => Conversion[Ident.type, Meta.IdentAPI] = _.api

  /** Matches a term identifier and returns its name */
  def unapply(tree: Ident): Some[String] = Some(tree.name)

}

// TODO (KR) : should this be an opaque type? trait? just a `.isWildcard`?
opaque type Wildcard <: Ident = Ident
object Wildcard {

  def api(using meta: Meta): Meta.WildcardAPI = meta.internal.wildcard
  given Meta => Conversion[Wildcard.type, Meta.WildcardAPI] = _.api

  @targetName("unapplyIdent")
  def unapply(x: Ident): Boolean = ??? // TODO (KR) :

  @targetName("unapplyWildcard")
  def unapply(x: Wildcard): true = true

}

trait Select private[meta] extends Ref {
  override type Self <: Select

  /** Qualifier of the `qualifier.name` */
  def qualifier: Term

  /** Name of this `Select` */
  def name: String

  /** Signature of this method */
  def signature: Option[Signature]

}
object Select {

  def api(using meta: Meta): Meta.SelectAPI = meta.internal.select
  given Meta => Conversion[Select.type, Meta.SelectAPI] = _.api

  /** Matches `<qualifier: Term>.<name: String>` */
  def unapply(x: Select): (Term, String) = (x.qualifier, x.name)

}

trait Literal private[meta] extends Term {
  override type Self <: Literal

  /** Value of this literal */
  def constant: Constant

}
object Literal {

  def api(using meta: Meta): Meta.LiteralAPI = meta.internal.literal
  given Meta => Conversion[Literal.type, Meta.LiteralAPI] = _.api

  /** Matches a literal constant */
  def unapply(x: Literal): Some[Constant] = Some(x.constant)

}

trait This private[meta] extends Term {
  override type Self <: This

  /**
    * Returns `C` if the underlying tree is of the form `C.this`
    *
    *  Otherwise, return `None`.
    */
  def id: Option[String]

}
object This {

  def api(using meta: Meta): Meta.ThisAPI = meta.internal.`this`
  given Meta => Conversion[This.type, Meta.ThisAPI] = _.api

  /** Matches `this` or `qual.this` and returns the name of `qual` */
  def unapply(x: This): Some[Option[String]] = Some(x.id)

}

trait New private[meta] extends Term {
  override type Self <: New

  /** Returns the type tree of this `new` */
  def tpt: TypeTree

}
object New {

  def api(using meta: Meta): Meta.NewAPI = meta.internal.`new`
  given Meta => Conversion[New.type, Meta.NewAPI] = _.api

  /** Matches `new <tpt: TypeTree>` */
  def unapply(x: New): Some[TypeTree] = Some(x.tpt)

}

trait NamedArg private[meta] extends Term {
  override type Self <: NamedArg

  /** The name part of `name = arg` */
  def name: String

  /** The argument part of `name = arg` */
  def value: Term

}
object NamedArg {

  def api(using meta: Meta): Meta.NamedArgAPI = meta.internal.namedArg
  given Meta => Conversion[NamedArg.type, Meta.NamedArgAPI] = _.api

  /** Matches a named argument `<name: String> = <value: Term>` */
  def unapply(x: NamedArg): (String, Term) = (x.name, x.value)

}

trait Apply private[meta] extends Term {
  override type Self <: Apply

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

  def api(using meta: Meta): Meta.ApplyAPI = meta.internal.apply
  given Meta => Conversion[Apply.type, Meta.ApplyAPI] = _.api

  /** Matches a function application `<fun: Term>(<args: List[Term]>)` */
  def unapply(x: Apply): (Term, List[Term]) = (x.fun, x.args)

}

trait TypeApply private[meta] extends Term {
  override type Self <: TypeApply

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

  def api(using meta: Meta): Meta.TypeApplyAPI = meta.internal.typeApply
  given Meta => Conversion[TypeApply.type, Meta.TypeApplyAPI] = _.api

  /** Matches a function type application `<fun: Term>[<args: List[TypeTree]>]` */
  def unapply(x: TypeApply): (Term, List[TypeTree]) = (x.fun, x.args)

}

trait Super private[meta] extends Term {
  override type Self <: Super

  def qualifier: Term

  def id: Option[String]

  def idPos: Position

}
object Super {

  def api(using meta: Meta): Meta.SuperAPI = meta.internal.`super`
  given Meta => Conversion[Super.type, Meta.SuperAPI] = _.api

  /** Matches a `<qualifier: Term>.super[<id: Option[Id]>` */
  def unapply(x: Super): (Term, Option[String]) = (x.qualifier, x.id)

}

trait Assign private[meta] extends Term {
  override type Self <: Assign

  def lhs: Term

  def rhs: Term

}
object Assign {

  def api(using meta: Meta): Meta.AssignAPI = meta.internal.assign
  given Meta => Conversion[Assign.type, Meta.AssignAPI] = _.api

  /** Matches an assignment `<lhs: Term> = <rhs: Term>` */
  def unapply(x: Assign): (Term, Term) = (x.lhs, x.rhs)

}

trait Block private[meta] extends Term {
  override type Self <: Block

  def statements: List[Statement]

  def expr: Term

}
object Block {

  def api(using meta: Meta): Meta.BlockAPI = meta.internal.block
  given Meta => Conversion[Block.type, Meta.BlockAPI] = _.api

  /** Matches a block `{ <statements: List[Statement]>; <expr: Term> }` */
  def unapply(x: Block): (List[Statement], Term) = (x.statements, x.expr)

}

trait Closure private[meta] extends Term {
  override type Self <: Closure

  // TODO (KR) : functions

}
object Closure {

  def api(using meta: Meta): Meta.ClosureAPI = meta.internal.closure
  given Meta => Conversion[Closure.type, Meta.ClosureAPI] = _.api

}

trait If private[meta] extends Term {
  override type Self <: If

  // TODO (KR) : functions

}
object If {

  def api(using meta: Meta): Meta.IfAPI = meta.internal.`if`
  given Meta => Conversion[If.type, Meta.IfAPI] = _.api

}

trait Match private[meta] extends Term {
  override type Self <: Match

  // TODO (KR) : functions

}
object Match {

  def api(using meta: Meta): Meta.MatchAPI = meta.internal.`match`
  given Meta => Conversion[Match.type, Meta.MatchAPI] = _.api

}

trait SummonFrom private[meta] extends Term {
  override type Self <: SummonFrom

  // TODO (KR) : functions

}
object SummonFrom {

  def api(using meta: Meta): Meta.SummonFromAPI = meta.internal.summonFrom
  given Meta => Conversion[SummonFrom.type, Meta.SummonFromAPI] = _.api

}

trait Try private[meta] extends Term {
  override type Self <: Try

  // TODO (KR) : functions

}
object Try {

  def api(using meta: Meta): Meta.TryAPI = meta.internal.`try`
  given Meta => Conversion[Try.type, Meta.TryAPI] = _.api

}

trait Return private[meta] extends Term {
  override type Self <: Return

  // TODO (KR) : functions

}
object Return {

  def api(using meta: Meta): Meta.ReturnAPI = meta.internal.`return`
  given Meta => Conversion[Return.type, Meta.ReturnAPI] = _.api

}

trait Repeated private[meta] extends Term {
  override type Self <: Repeated

  // TODO (KR) : functions

}
object Repeated {

  def api(using meta: Meta): Meta.RepeatedAPI = meta.internal.repeated
  given Meta => Conversion[Repeated.type, Meta.RepeatedAPI] = _.api

}

trait Inlined private[meta] extends Term {
  override type Self <: Inlined

  // TODO (KR) : functions

}
object Inlined {

  def api(using meta: Meta): Meta.InlinedAPI = meta.internal.inlined
  given Meta => Conversion[Inlined.type, Meta.InlinedAPI] = _.api

}

trait SelectOuter private[meta] extends Term {
  override type Self <: SelectOuter

  // TODO (KR) : functions

}
object SelectOuter {

  def api(using meta: Meta): Meta.SelectOuterAPI = meta.internal.selectOuter
  given Meta => Conversion[SelectOuter.type, Meta.SelectOuterAPI] = _.api

}

trait While private[meta] extends Term {
  override type Self <: While

  // TODO (KR) : functions

}
object While {

  def api(using meta: Meta): Meta.WhileAPI = meta.internal.`while`
  given Meta => Conversion[While.type, Meta.WhileAPI] = _.api

}

sealed trait TypedOrTest extends Tree {
  override type Self <: TypedOrTest

  // TODO (KR) : functions

}
object TypedOrTest {

  def api(using meta: Meta): Meta.TypedOrTestAPI = meta.internal.typedOrTest
  given Meta => Conversion[TypedOrTest.type, Meta.TypedOrTestAPI] = _.api

}

trait Typed private[meta] extends Term, TypedOrTest {
  override type Self <: Typed

  // TODO (KR) : functions

}
object Typed {

  def api(using meta: Meta): Meta.TypedAPI = meta.internal.typed
  given Meta => Conversion[Typed.type, Meta.TypedAPI] = _.api

}

trait Bind private[meta] extends Tree {
  override type Self <: Bind

  // TODO (KR) : functions

}
object Bind {

  def api(using meta: Meta): Meta.BindAPI = meta.internal.bind
  given Meta => Conversion[Bind.type, Meta.BindAPI] = _.api

}

trait Unapply private[meta] extends Tree {
  override type Self <: Unapply

  // TODO (KR) : functions

}
object Unapply {

  def api(using meta: Meta): Meta.UnapplyAPI = meta.internal.unapply
  given Meta => Conversion[Unapply.type, Meta.UnapplyAPI] = _.api

}

trait Alternatives private[meta] extends Tree {
  override type Self <: Alternatives

  // TODO (KR) : functions

}
object Alternatives {

  def api(using meta: Meta): Meta.AlternativesAPI = meta.internal.alternatives
  given Meta => Conversion[Alternatives.type, Meta.AlternativesAPI] = _.api

}

trait CaseDef private[meta] extends Tree {
  override type Self <: CaseDef

  // TODO (KR) : functions

}
object CaseDef {

  def api(using meta: Meta): Meta.CaseDefAPI = meta.internal.caseDef
  given Meta => Conversion[CaseDef.type, Meta.CaseDefAPI] = _.api

}

trait TypeCaseDef private[meta] extends Tree {
  override type Self <: TypeCaseDef

  // TODO (KR) : functions

}
object TypeCaseDef {

  def api(using meta: Meta): Meta.TypeCaseDefAPI = meta.internal.typeCaseDef
  given Meta => Conversion[TypeCaseDef.type, Meta.TypeCaseDefAPI] = _.api

}

sealed trait TypeTree extends Tree {
  override type Self <: TypeTree

  // TODO (KR) : functions

}
object TypeTree {

  def api(using meta: Meta): Meta.TypeTreeAPI = meta.internal.typeTree
  given Meta => Conversion[TypeTree.type, Meta.TypeTreeAPI] = _.api

}

trait Inferred private[meta] extends TypeTree {
  override type Self <: Inferred

  // TODO (KR) : functions

}
object Inferred {

  def api(using meta: Meta): Meta.InferredAPI = meta.internal.inferred
  given Meta => Conversion[Inferred.type, Meta.InferredAPI] = _.api

}

trait TypeIdent private[meta] extends TypeTree {
  override type Self <: TypeIdent

  // TODO (KR) : functions

}
object TypeIdent {

  def api(using meta: Meta): Meta.TypeIdentAPI = meta.internal.typeIdent
  given Meta => Conversion[TypeIdent.type, Meta.TypeIdentAPI] = _.api

}

trait TypeSelect private[meta] extends TypeTree {
  override type Self <: TypeSelect

  // TODO (KR) : functions

}
object TypeSelect {

  def api(using meta: Meta): Meta.TypeSelectAPI = meta.internal.typeSelect
  given Meta => Conversion[TypeSelect.type, Meta.TypeSelectAPI] = _.api

}

trait TypeProjection private[meta] extends TypeTree {
  override type Self <: TypeProjection

  // TODO (KR) : functions

}
object TypeProjection {

  def api(using meta: Meta): Meta.TypeProjectionAPI = meta.internal.typeProjection
  given Meta => Conversion[TypeProjection.type, Meta.TypeProjectionAPI] = _.api

}

trait Singleton private[meta] extends TypeTree {
  override type Self <: Singleton

  // TODO (KR) : functions

}
object Singleton {

  def api(using meta: Meta): Meta.SingletonAPI = meta.internal.singleton
  given Meta => Conversion[Singleton.type, Meta.SingletonAPI] = _.api

}

trait Refined private[meta] extends TypeTree {
  override type Self <: Refined

  // TODO (KR) : functions

}
object Refined {

  def api(using meta: Meta): Meta.RefinedAPI = meta.internal.refined
  given Meta => Conversion[Refined.type, Meta.RefinedAPI] = _.api

}

trait Applied private[meta] extends TypeTree {
  override type Self <: Applied

  // TODO (KR) : functions

}
object Applied {

  def api(using meta: Meta): Meta.AppliedAPI = meta.internal.applied
  given Meta => Conversion[Applied.type, Meta.AppliedAPI] = _.api

}

trait Annotated private[meta] extends TypeTree {
  override type Self <: Annotated

  // TODO (KR) : functions

}
object Annotated {

  def api(using meta: Meta): Meta.AnnotatedAPI = meta.internal.annotated
  given Meta => Conversion[Annotated.type, Meta.AnnotatedAPI] = _.api

}

trait MatchTypeTree private[meta] extends TypeTree {
  override type Self <: MatchTypeTree

  // TODO (KR) : functions

}
object MatchTypeTree {

  def api(using meta: Meta): Meta.MatchTypeTreeAPI = meta.internal.matchTypeTree
  given Meta => Conversion[MatchTypeTree.type, Meta.MatchTypeTreeAPI] = _.api

}

trait ByName private[meta] extends TypeTree {
  override type Self <: ByName

  // TODO (KR) : functions

}
object ByName {

  def api(using meta: Meta): Meta.ByNameAPI = meta.internal.byName
  given Meta => Conversion[ByName.type, Meta.ByNameAPI] = _.api

}

trait LambdaTypeTree private[meta] extends TypeTree {
  override type Self <: LambdaTypeTree

  // TODO (KR) : functions

}
object LambdaTypeTree {

  def api(using meta: Meta): Meta.LambdaTypeTreeAPI = meta.internal.lambdaTypeTree
  given Meta => Conversion[LambdaTypeTree.type, Meta.LambdaTypeTreeAPI] = _.api

}

trait TypeBind private[meta] extends TypeTree {
  override type Self <: TypeBind

  // TODO (KR) : functions

}
object TypeBind {

  def api(using meta: Meta): Meta.TypeBindAPI = meta.internal.typeBind
  given Meta => Conversion[TypeBind.type, Meta.TypeBindAPI] = _.api

}

trait TypeBlock private[meta] extends TypeTree {
  override type Self <: TypeBlock

  // TODO (KR) : functions

}
object TypeBlock {

  def api(using meta: Meta): Meta.TypeBlockAPI = meta.internal.typeBlock
  given Meta => Conversion[TypeBlock.type, Meta.TypeBlockAPI] = _.api

}

trait TypeBoundsTree private[meta] extends Tree {
  override type Self <: TypeBoundsTree

  // TODO (KR) : functions

}
object TypeBoundsTree {

  def api(using meta: Meta): Meta.TypeBoundsTreeAPI = meta.internal.typeBoundsTree
  given Meta => Conversion[TypeBoundsTree.type, Meta.TypeBoundsTreeAPI] = _.api

}

trait WildcardTypeTree private[meta] extends Tree {
  override type Self <: WildcardTypeTree

  // TODO (KR) : functions

}
object WildcardTypeTree {

  def api(using meta: Meta): Meta.WildcardTypeTreeAPI = meta.internal.wildcardTypeTree
  given Meta => Conversion[WildcardTypeTree.type, Meta.WildcardTypeTreeAPI] = _.api

}
