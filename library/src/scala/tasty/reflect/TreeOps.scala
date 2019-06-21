package scala.tasty
package reflect

trait TreeOps extends Core {

  // ----- Tree -----------------------------------------------------

  implicit class TreeAPI(self: Tree) {
    /** Position in the source code */
    def pos(implicit ctx: Context): Position = kernel.Tree_pos(self)

    def symbol(implicit ctx: Context): Symbol = kernel.Tree_symbol(self)
  }

  object IsPackageClause {
    def unapply(tree: Tree)(implicit ctx: Context): Option[PackageClause] =
      kernel.matchPackageClause(tree)
  }

  object PackageClause {
    def apply(pid: Ref, stats: List[Tree])(implicit ctx: Context): PackageClause =
      kernel.PackageClause_apply(pid, stats)
    def copy(original: PackageClause)(pid: Ref, stats: List[Tree])(implicit ctx: Context): PackageClause =
      kernel.PackageClause_copy(original)(pid, stats)
    def unapply(tree: Tree)(implicit ctx: Context): Option[(Ref, List[Tree])] =
      kernel.matchPackageClause(tree).map(x => (x.pid, x.stats))
  }

  implicit class PackageClauseAPI(self: PackageClause) {
    def pid(implicit ctx: Context): Ref = kernel.PackageClause_pid(self)
    def stats(implicit ctx: Context): List[Tree] = kernel.PackageClause_stats(self)
  }

  object IsImport {
    def unapply(tree: Tree)(implicit ctx: Context): Option[Import] =
      kernel.matchImport(tree)
  }

  object Import {
    def apply(importImplied: Boolean, expr: Term, selectors: List[ImportSelector])(implicit ctx: Context): Import =
      kernel.Import_apply(importImplied, expr, selectors)
    def copy(original: Import)(importImplied: Boolean, expr: Term, selectors: List[ImportSelector])(implicit ctx: Context): Import =
      kernel.Import_copy(original)(importImplied, expr, selectors)
    def unapply(tree: Tree)(implicit ctx: Context): Option[(Boolean, Term, List[ImportSelector])] =
      kernel.matchImport(tree).map(x => (x.importImplied, x.expr, x.selectors))
  }

  implicit class ImportAPI(self: Import)  {
    def importImplied: Boolean = kernel.Import_implied(self)
    def expr(implicit ctx: Context): Term = kernel.Import_expr(self)
    def selectors(implicit ctx: Context): List[ImportSelector] =
      kernel.Import_selectors(self)
  }

  object IsStatement {
    /** Matches any Statement and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Statement] = kernel.matchStatement(tree)
  }

  // ----- Definitions ----------------------------------------------

  object IsDefinition {
    def unapply(tree: Tree)(implicit ctx: Context): Option[Definition] = kernel.matchDefinition(tree)
  }

  implicit class DefinitionAPI(self: Definition) {
    def name(implicit ctx: Context): String = kernel.Definition_name(self)
  }

  // ClassDef

  object IsClassDef {
    def unapply(tree: Tree)(implicit ctx: Context): Option[ClassDef] = kernel.matchClassDef(tree)
  }

  object ClassDef {
    // TODO def apply(name: String, constr: DefDef, parents: List[TermOrTypeTree], selfOpt: Option[ValDef], body: List[Statement])(implicit ctx: Context): ClassDef
    def copy(original: ClassDef)(name: String, constr: DefDef, parents: List[Tree /* Term | TypeTree */], derived: List[TypeTree], selfOpt: Option[ValDef], body: List[Statement])(implicit ctx: Context): ClassDef =
      kernel.ClassDef_copy(original)(name, constr, parents, derived, selfOpt, body)
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, DefDef, List[Tree /* Term | TypeTree */], List[TypeTree], Option[ValDef], List[Statement])] =
      kernel.matchClassDef(tree).map(x => (x.name, x.constructor, x.parents, x.derived, x.self, x.body))
  }

  implicit class ClassDefAPI(self: ClassDef) {
    def constructor(implicit ctx: Context): DefDef = kernel.ClassDef_constructor(self)
    def parents(implicit ctx: Context): List[Tree /* Term | TypeTree */] = kernel.ClassDef_parents(self)
    def derived(implicit ctx: Context): List[TypeTree] = kernel.ClassDef_derived(self)
    def self(implicit ctx: Context): Option[ValDef] = kernel.ClassDef_self(self)
    def body(implicit ctx: Context): List[Statement] = kernel.ClassDef_body(self)
    def symbol(implicit ctx: Context): ClassDefSymbol = kernel.ClassDef_symbol(self)
  }

  // DefDef

  object IsDefDef {
    def unapply(tree: Tree)(implicit ctx: Context): Option[DefDef] = kernel.matchDefDef(tree)
  }

  object DefDef {
    def apply(symbol: DefDefSymbol, rhsFn: List[Type] => List[List[Term]] => Option[Term])(implicit ctx: Context): DefDef =
      kernel.DefDef_apply(symbol, rhsFn)
    def copy(original: DefDef)(name: String, typeParams: List[TypeDef], paramss: List[List[ValDef]], tpt: TypeTree, rhs: Option[Term])(implicit ctx: Context): DefDef =
      kernel.DefDef_copy(original)(name, typeParams, paramss, tpt, rhs)
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, List[TypeDef], List[List[ValDef]], TypeTree, Option[Term])] =
      kernel.matchDefDef(tree).map(x => (x.name, x.typeParams, x.paramss, x.returnTpt, x.rhs))
  }

  implicit class DefDefAPI(self: DefDef) {
    def typeParams(implicit ctx: Context): List[TypeDef] = kernel.DefDef_typeParams(self)
    def paramss(implicit ctx: Context): List[List[ValDef]] = kernel.DefDef_paramss(self)
    def returnTpt(implicit ctx: Context): TypeTree = kernel.DefDef_returnTpt(self) // TODO rename to tpt
    def rhs(implicit ctx: Context): Option[Term] = kernel.DefDef_rhs(self)
    def symbol(implicit ctx: Context): DefDefSymbol = kernel.DefDef_symbol(self)
  }

  // ValDef

  object IsValDef {
    def unapply(tree: Tree)(implicit ctx: Context): Option[ValDef] = kernel.matchValDef(tree)
  }

  object ValDef {
    def apply(symbol: ValDefSymbol, rhs: Option[Term])(implicit ctx: Context): ValDef =
      kernel.ValDef_apply(symbol, rhs)
    def copy(original: ValDef)(name: String, tpt: TypeTree, rhs: Option[Term])(implicit ctx: Context): ValDef =
      kernel.ValDef_copy(original)(name, tpt, rhs)
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, TypeTree, Option[Term])] =
      kernel.matchValDef(tree).map(x => (x.name, x.tpt, x.rhs))
  }

  implicit class ValDefAPI(self: ValDef) {
    def tpt(implicit ctx: Context): TypeTree = kernel.ValDef_tpt(self)
    def rhs(implicit ctx: Context): Option[Term] = kernel.ValDef_rhs(self)
    def symbol(implicit ctx: Context): ValDefSymbol = kernel.ValDef_symbol(self)
  }

  // TypeDef

  object IsTypeDef {
    def unapply(tree: Tree)(implicit ctx: Context): Option[TypeDef] = kernel.matchTypeDef(tree)
  }

  object TypeDef {
    def apply(symbol: TypeDefSymbol)(implicit ctx: Context): TypeDef =
      kernel.TypeDef_apply(symbol)
    def copy(original: TypeDef)(name: String, rhs: Tree /*TypeTree | TypeBoundsTree*/)(implicit ctx: Context): TypeDef =
      kernel.TypeDef_copy(original)(name, rhs)
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, Tree /*TypeTree | TypeBoundsTree*/ /* TypeTree | TypeBoundsTree */)] =
      kernel.matchTypeDef(tree).map(x => (x.name, x.rhs))
  }

  implicit class TypeDefAPI(self: TypeDef) {
    def rhs(implicit ctx: Context): Tree /*TypeTree | TypeBoundsTree*/ = kernel.TypeDef_rhs(self)
    def symbol(implicit ctx: Context): TypeDefSymbol = kernel.TypeDef_symbol(self)
  }

  // PackageDef

  object IsPackageDef {
    def unapply(tree: Tree)(implicit ctx: Context): Option[PackageDef] =
      kernel.matchPackageDef(tree)
  }

  implicit class PackageDefAPI(self: PackageDef) {
    def owner(implicit ctx: Context): PackageDef = kernel.PackageDef_owner(self)
    def members(implicit ctx: Context): List[Statement] = kernel.PackageDef_members(self)
    def symbol(implicit ctx: Context): PackageDefSymbol = kernel.PackageDef_symbol(self)
  }

  object PackageDef {
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, PackageDef)] =
      kernel.matchPackageDef(tree).map(x => (x.name, x.owner))
  }

  // ----- Terms ----------------------------------------------------

  implicit class TermAPI(self: Term) {
    def tpe(implicit ctx: Context): Type = kernel.Term_tpe(self)
    def pos(implicit ctx: Context): Position = kernel.Term_pos(self)
    def underlyingArgument(implicit ctx: Context): Term = kernel.Term_underlyingArgument(self)
    def underlying(implicit ctx: Context): Term = kernel.Term_underlying(self)

    /** A unary apply node with given argument: `tree(arg)` */
    def appliedTo(arg: Term)(implicit ctx: Context): Term =
      appliedToArgs(arg :: Nil)

    /** An apply node with given arguments: `tree(arg, args0, ..., argsN)` */
    def appliedTo(arg: Term, args: Term*)(implicit ctx: Context): Term =
      appliedToArgs(arg :: args.toList)

    /** An apply node with given argument list `tree(args(0), ..., args(args.length - 1))` */
    def appliedToArgs(args: List[Term])(implicit ctx: Context): Apply =
      Apply(self, args)

    /** The current tree applied to given argument lists:
     *  `tree (argss(0)) ... (argss(argss.length -1))`
     */
    def appliedToArgss(argss: List[List[Term]])(implicit ctx: Context): Term =
      ((self: Term) /: argss)(Apply(_, _))

    /** The current tree applied to (): `tree()` */
    def appliedToNone(implicit ctx: Context): Apply = appliedToArgs(Nil)

    /** The current tree applied to given type argument: `tree[targ]` */
    def appliedToType(targ: Type)(implicit ctx: Context): Term =
      appliedToTypes(targ :: Nil)

    /** The current tree applied to given type arguments: `tree[targ0, ..., targN]` */
    def appliedToTypes(targs: List[Type])(implicit ctx: Context): Term =
      appliedToTypeTrees(targs map (Inferred(_)))

    /** The current tree applied to given type argument list: `tree[targs(0), ..., targs(targs.length - 1)]` */
    def appliedToTypeTrees(targs: List[TypeTree])(implicit ctx: Context): Term =
      if (targs.isEmpty) self else TypeApply(self, targs)

    /** A select node that selects the given symbol.
     */
    def select(sym: Symbol)(implicit ctx: Context): Select = Select(self, sym)
  }

  object IsTerm {
    /** Matches any term */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Term] =
      kernel.matchTerm(tree)
  }

  object IsRef {
    /** Matches any Ref and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Ref] = kernel.matchRef(tree)
  }

  object Ref {

    /** Create a reference tree */
    def apply(sym: Symbol)(implicit ctx: Context): Ref =
      kernel.Ref_apply(sym)

    // TODO def copy(original: Tree)(name: String)(implicit ctx: Context): Ref

  }

  object IsIdent {
    /** Matches any Ident and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Ident] = kernel.matchIdent(tree)
  }

  implicit class IdentAPI(self: Ident) {
    def name(implicit ctx: Context): String = kernel.Ident_name(self)
  }

  /** Scala term identifier */
  object Ident {
    def apply(tmref: TermRef)(implicit ctx: Context): Term =
      kernel.Ident_apply(tmref)

    def copy(original: Tree)(name: String)(implicit ctx: Context): Ident =
      kernel.Ident_copy(original)(name)

    /** Matches a term identifier and returns its name */
    def unapply(tree: Tree)(implicit ctx: Context): Option[String] =
      kernel.matchIdent(tree).map(_.name)
  }

  object IsSelect {
    /** Matches any Select and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Select] = kernel.matchSelect(tree)
  }

  /** Scala term selection */
  object Select {
    /** Select a term member by symbol */
    def apply(qualifier: Term, symbol: Symbol)(implicit ctx: Context): Select =
      kernel.Select_apply(qualifier, symbol)

    /** Select a field or a non-overloaded method by name
     *
     *  @note The method will produce an assertion error if the selected
     *        method is overloaded. The method `overloaded` should be used
     *        in that case.
     */
    def unique(qualifier: Term, name: String)(implicit ctx: Context): Select =
      kernel.Select_unique(qualifier, name)

    // TODO rename, this returns an Apply and not a Select
    /** Call an overloaded method with the given type and term parameters */
    def overloaded(qualifier: Term, name: String, targs: List[Type], args: List[Term])(implicit ctx: Context): Apply =
      kernel.Select_overloaded(qualifier, name, targs, args)

    def copy(original: Tree)(qualifier: Term, name: String)(implicit ctx: Context): Select =
      kernel.Select_copy(original)(qualifier, name)

    /** Matches `<qualifier: Term>.<name: String>` */
    def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, String)] =
      kernel.matchSelect(tree).map(x => (x.qualifier, x.name))
  }

  implicit class SelectAPI(self: Select) {
    def qualifier(implicit ctx: Context): Term = kernel.Select_qualifier(self)
    def name(implicit ctx: Context): String = kernel.Select_name(self)
    def signature(implicit ctx: Context): Option[Signature] = kernel.Select_signature(self)
  }

  object IsLiteral {
    /** Matches any Literal and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Literal] = kernel.matchLiteral(tree)
  }

  /** Scala literal constant */
  object Literal {

    /** Create a literal constant */
    def apply(constant: Constant)(implicit ctx: Context): Literal =
      kernel.Literal_apply(constant)

    def copy(original: Tree)(constant: Constant)(implicit ctx: Context): Literal =
      kernel.Literal_copy(original)(constant)

    /** Matches a literal constant */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Constant] =
      kernel.matchLiteral(tree).map(_.constant)
  }

  implicit class LiteralAPI(self:  Literal) {
    def constant(implicit ctx: Context): Constant = kernel.Literal_constant(self)
  }

  object IsThis {
    /** Matches any This and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[This] = kernel.matchThis(tree)
  }

  /** Scala `this` or `this[id]` */
  object This {

    /** Create a `this[<id: Id]>` */
    def apply(cls: ClassDefSymbol)(implicit ctx: Context): This =
      kernel.This_apply(cls)

    def copy(original: Tree)(qual: Option[Id])(implicit ctx: Context): This =
      kernel.This_copy(original)(qual)

    /** Matches `this[<id: Option[Id]>` */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Option[Id]] =
      kernel.matchThis(tree).map(_.id)

  }

  implicit class ThisAPI(self:  This) {
    def id(implicit ctx: Context): Option[Id] = kernel.This_id(self)
  }

  object IsNew {
    /** Matches any New and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[New] = kernel.matchNew(tree)
  }

  /** Scala `new` */
  object New {

    /** Create a `new <tpt: TypeTree>` */
    def apply(tpt: TypeTree)(implicit ctx: Context): New =
      kernel.New_apply(tpt)

    def copy(original: Tree)(tpt: TypeTree)(implicit ctx: Context): New =
      kernel.New_copy(original)(tpt)

    /** Matches a `new <tpt: TypeTree>` */
    def unapply(tree: Tree)(implicit ctx: Context): Option[TypeTree] =
      kernel.matchNew(tree).map(_.tpt)
  }

  implicit class NewAPI(self: New) {
    def tpt(implicit ctx: Context): TypeTree = kernel.New_tpt(self)
  }

  object IsNamedArg {
    /** Matches any NamedArg and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[NamedArg] = kernel.matchNamedArg(tree)
  }

  /** Scala named argument `x = y` in argument position */
  object NamedArg {

    /** Create a named argument `<name: String> = <value: Term>` */
    def apply(name: String, arg: Term)(implicit ctx: Context): NamedArg =
      kernel.NamedArg_apply(name, arg)

    def copy(original: NamedArg)(name: String, arg: Term)(implicit ctx: Context): NamedArg =
      kernel.NamedArg_copy(original)(name, arg)

    /** Matches a named argument `<name: String> = <value: Term>` */
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, Term)] =
      kernel.matchNamedArg(tree).map(x => (x.name, x.value))

  }

  implicit class NamedArgAPI(self: NamedArg) {
    def name(implicit ctx: Context): String = kernel.NamedArg_name(self)
    def value(implicit ctx: Context): Term = kernel.NamedArg_value(self)
  }

  object IsApply {
    /** Matches any Apply and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Apply] = kernel.matchApply(tree)
  }

  /** Scala parameter application */
  object Apply {

    /** Create a function application `<fun: Term>(<args: List[Term]>)` */
    def apply(fun: Term, args: List[Term])(implicit ctx: Context): Apply =
      kernel.Apply_apply(fun, args)

    def copy(original: Tree)(fun: Term, args: List[Term])(implicit ctx: Context): Apply =
      kernel.Apply_copy(original)(fun, args)

    /** Matches a function application `<fun: Term>(<args: List[Term]>)` */
    def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, List[Term])] =
      kernel.matchApply(tree).map(x => (x.fun, x.args))
  }

  implicit class ApplyAPI(self: Apply) {
    def fun(implicit ctx: Context): Term = kernel.Apply_fun(self)
    def args(implicit ctx: Context): List[Term] = kernel.Apply_args(self)
  }

  object IsTypeApply {
    /** Matches any TypeApply and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[TypeApply] =
      kernel.matchTypeApply(tree)
  }

  /** Scala type parameter application */
  object TypeApply {

    /** Create a function type application `<fun: Term>[<args: List[TypeTree]>]` */
    def apply(fun: Term, args: List[TypeTree])(implicit ctx: Context): TypeApply =
      kernel.TypeApply_apply(fun, args)

    def copy(original: Tree)(fun: Term, args: List[TypeTree])(implicit ctx: Context): TypeApply =
      kernel.TypeApply_copy(original)(fun, args)

    /** Matches a function type application `<fun: Term>[<args: List[TypeTree]>]` */
    def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, List[TypeTree])] =
      kernel.matchTypeApply(tree).map(x => (x.fun, x.args))

  }

  implicit class TypeApplyAPI(self: TypeApply) {
    def fun(implicit ctx: Context): Term = kernel.TypeApply_fun(self)
    def args(implicit ctx: Context): List[TypeTree] = kernel.TypeApply_args(self)
  }

  object IsSuper {
    /** Matches any Super and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Super] = kernel.matchSuper(tree)
  }

  /** Scala `x.super` or `x.super[id]` */
  object Super {

    /** Creates a `<qualifier: Term>.super[<id: Option[Id]>` */
    def apply(qual: Term, mix: Option[Id])(implicit ctx: Context): Super =
      kernel.Super_apply(qual, mix)

    def copy(original: Tree)(qual: Term, mix: Option[Id])(implicit ctx: Context): Super =
      kernel.Super_copy(original)(qual, mix)

    /** Matches a `<qualifier: Term>.super[<id: Option[Id]>` */
    def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Option[Id])] =
      kernel.matchSuper(tree).map(x => (x.qualifier, x.id))
  }

  implicit class SuperAPI(self: Super) {
    def qualifier(implicit ctx: Context): Term = kernel.Super_qualifier(self)
    def id(implicit ctx: Context): Option[Id] = kernel.Super_id(self)
  }

  object IsTyped {
    /** Matches any Typed and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Typed] = kernel.matchTyped(tree)
  }

  /** Scala ascription `x: T` */
  object Typed {

    /** Create a type ascription `<x: Term>: <tpt: TypeTree>` */
    def apply(expr: Term, tpt: TypeTree)(implicit ctx: Context): Typed =
      kernel.Typed_apply(expr, tpt)

    def copy(original: Tree)(expr: Term, tpt: TypeTree)(implicit ctx: Context): Typed =
      kernel.Typed_copy(original)(expr, tpt)

    /** Matches `<expr: Term>: <tpt: TypeTree>` */
    def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, TypeTree)] =
      kernel.matchTyped(tree).map(x => (x.expr, x.tpt))

  }

  implicit class TypedAPI(self: Typed) {
    def expr(implicit ctx: Context): Term = kernel.Typed_expr(self)
    def tpt(implicit ctx: Context): TypeTree = kernel.Typed_tpt(self)
  }

  object IsAssign {
    /** Matches any Assign and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Assign] = kernel.matchAssign(tree)
  }

  /** Scala assign `x = y` */
  object Assign {

    /** Create an assignment `<lhs: Term> = <rhs: Term>` */
    def apply(lhs: Term, rhs: Term)(implicit ctx: Context): Assign =
      kernel.Assign_apply(lhs, rhs)

    def copy(original: Tree)(lhs: Term, rhs: Term)(implicit ctx: Context): Assign =
      kernel.Assign_copy(original)(lhs, rhs)

    /** Matches an assignment `<lhs: Term> = <rhs: Term>` */
    def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Term)] =
      kernel.matchAssign(tree).map(x => (x.lhs, x.rhs))
  }

  implicit class AssignAPI(self: Assign) {
    def lhs(implicit ctx: Context): Term = kernel.Assign_lhs(self)
    def rhs(implicit ctx: Context): Term = kernel.Assign_rhs(self)
  }

  object IsBlock {
    /** Matches any Block and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Block] = kernel.matchBlock(tree)
  }

  /** Scala code block `{ stat0; ...; statN; expr }` term */
  object Block {

    /** Creates a block `{ <statements: List[Statement]>; <expr: Term> }` */
    def apply(stats: List[Statement], expr: Term)(implicit ctx: Context): Block =
      kernel.Block_apply(stats, expr)

    def copy(original: Tree)(stats: List[Statement], expr: Term)(implicit ctx: Context): Block =
      kernel.Block_copy(original)(stats, expr)

    /** Matches a block `{ <statements: List[Statement]>; <expr: Term> }` */
    def unapply(tree: Tree)(implicit ctx: Context): Option[(List[Statement], Term)] =
      kernel.matchBlock(tree).map(x => (x.statements, x.expr))
  }

  implicit class BlockAPI(self: Block) {
    def statements(implicit ctx: Context): List[Statement] = kernel.Block_statements(self)
    def expr(implicit ctx: Context): Term = kernel.Block_expr(self)
  }

  object IsLambda {
    /** Matches any Lambda and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Lambda] = kernel.matchLambda(tree)
  }

  object Lambda {

    def apply(meth: Term, tpt: Option[TypeTree])(implicit ctx: Context): Lambda =
      kernel.Lambda_apply(meth, tpt)

    def copy(original: Tree)(meth: Tree, tpt: Option[TypeTree])(implicit ctx: Context): Lambda =
      kernel.Lambda_copy(original)(meth, tpt)

    def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Option[TypeTree])] =
      kernel.matchLambda(tree).map(x => (x.meth, x.tptOpt))
  }

  implicit class LambdaAPI(self: Lambda) {
    def meth(implicit ctx: Context): Term = kernel.Lambda_meth(self)
    def tptOpt(implicit ctx: Context): Option[TypeTree] = kernel.Lambda_tptOpt(self)
  }

  object IsIf {
    /** Matches any If and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[If] = kernel.matchIf(tree)
  }

  /** Scala `if`/`else` term */
  object If {

    /** Create an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>` */
    def apply(cond: Term, thenp: Term, elsep: Term)(implicit ctx: Context): If =
      kernel.If_apply(cond, thenp, elsep)

    def copy(original: Tree)(cond: Term, thenp: Term, elsep: Term)(implicit ctx: Context): If =
      kernel.If_copy(original)(cond, thenp, elsep)

    /** Matches an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>` */
    def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Term, Term)] =
      kernel.matchIf(tree).map(x => (x.cond, x.thenp, x.elsep))

  }

  implicit class IfAPI(self: If) {
    def cond(implicit ctx: Context): Term = kernel.If_cond(self)
    def thenp(implicit ctx: Context): Term = kernel.If_thenp(self)
    def elsep(implicit ctx: Context): Term = kernel.If_elsep(self)
  }

  object IsMatch {
    /** Matches any Match and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Match] = kernel.matchMatch(tree)
  }

  /** Scala `match` term */
  object Match {

    /** Creates a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }` */
    def apply(selector: Term, cases: List[CaseDef])(implicit ctx: Context): Match =
      kernel.Match_apply(selector, cases)

    def copy(original: Tree)(selector: Term, cases: List[CaseDef])(implicit ctx: Context): Match =
      kernel.Match_copy(original)(selector, cases)

    /** Matches a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }` */
    def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, List[CaseDef])] =
      kernel.matchMatch(tree).map(x => (x.scrutinee, x.cases))

  }

  implicit class MatchAPI(self: Match) {
    def scrutinee(implicit ctx: Context): Term = kernel.Match_scrutinee(self)
    def cases(implicit ctx: Context): List[CaseDef] = kernel.Match_cases(self)
  }

  object IsImplicitMatch {
    /** Matches any ImpliedMatch and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[ImpliedMatch] = kernel.matchImplicitMatch(tree)
  }

  /** Scala implicit `match` term */
  object ImpliedMatch {

    /** Creates a pattern match `delegate match { <cases: List[CaseDef]> }` */
    def apply(cases: List[CaseDef])(implicit ctx: Context): ImpliedMatch =
      kernel.ImplicitMatch_apply(cases)

    def copy(original: Tree)(cases: List[CaseDef])(implicit ctx: Context): ImpliedMatch =
      kernel.ImplicitMatch_copy(original)(cases)

    /** Matches a pattern match `delegate match { <cases: List[CaseDef]> }` */
    def unapply(tree: Tree)(implicit ctx: Context): Option[List[CaseDef]] =
      kernel.matchImplicitMatch(tree).map(_.cases)

  }

  implicit class ImplicitMatchAPI(self: ImpliedMatch) {
    def cases(implicit ctx: Context): List[CaseDef] = kernel.ImplicitMatch_cases(self)
  }

  object IsTry {
    /** Matches any Try and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Try] = kernel.matchTry(tree)
  }

  /** Scala `try`/`catch`/`finally` term */
  object Try {

    /** Create a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>` */
    def apply(expr: Term, cases: List[CaseDef], finalizer: Option[Term])(implicit ctx: Context): Try =
      kernel.Try_apply(expr, cases, finalizer)

    def copy(original: Tree)(expr: Term, cases: List[CaseDef], finalizer: Option[Term])(implicit ctx: Context): Try =
      kernel.Try_copy(original)(expr, cases, finalizer)

    /** Matches a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>` */
    def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, List[CaseDef], Option[Term])] =
      kernel.matchTry(tree).map(x => (x.body, x.cases, x.finalizer))

  }

  implicit class TryAPI(self: Try) {
    def body(implicit ctx: Context): Term = kernel.Try_body(self)
    def cases(implicit ctx: Context): List[CaseDef] = kernel.Try_cases(self)
    def finalizer(implicit ctx: Context): Option[Term] = kernel.Try_finalizer(self)
  }

  object IsReturn {
    /** Matches any Return and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Return] = kernel.matchReturn(tree)
  }

  /** Scala local `return` */
  object Return {

    /** Creates `return <expr: Term>` */
    def apply(expr: Term)(implicit ctx: Context): Return =
      kernel.Return_apply(expr)

    def copy(original: Tree)(expr: Term)(implicit ctx: Context): Return =
      kernel.Return_copy(original)(expr)

    /** Matches `return <expr: Term>` */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Term] =
      kernel.matchReturn(tree).map(_.expr)

  }

  implicit class ReturnAPI(self: Return) {
    def expr(implicit ctx: Context): Term = kernel.Return_expr(self)
  }

  object IsRepeated {
    /** Matches any Repeated and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Repeated] = kernel.matchRepeated(tree)
  }

  object Repeated {

    def apply(elems: List[Term], tpt: TypeTree)(implicit ctx: Context): Repeated =
      kernel.Repeated_apply(elems, tpt)

    def copy(original: Tree)(elems: List[Term], tpt: TypeTree)(implicit ctx: Context): Repeated =
      kernel.Repeated_copy(original)(elems, tpt)

    def unapply(tree: Tree)(implicit ctx: Context): Option[(List[Term], TypeTree)] =
      kernel.matchRepeated(tree).map(x => (x.elems, x.elemtpt))

  }

  implicit class RepeatedAPI(self: Repeated) {
    def elems(implicit ctx: Context): List[Term] = kernel.Repeated_elems(self)
    def elemtpt(implicit ctx: Context): TypeTree = kernel.Repeated_elemtpt(self)
  }

  object IsInlined {
    /** Matches any Inlined and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Inlined] = kernel.matchInlined(tree)
  }

  object Inlined {

    def apply(call: Option[Tree /* Term | TypeTree */], bindings: List[Definition], expansion: Term)(implicit ctx: Context): Inlined =
      kernel.Inlined_apply(call, bindings, expansion)

    def copy(original: Tree)(call: Option[Tree /* Term | TypeTree */], bindings: List[Definition], expansion: Term)(implicit ctx: Context): Inlined =
      kernel.Inlined_copy(original)(call, bindings, expansion)

    def unapply(tree: Tree)(implicit ctx: Context): Option[(Option[Tree /* Term | TypeTree */], List[Definition], Term)] =
      kernel.matchInlined(tree).map(x => (x.call, x.bindings, x.body))

  }

  implicit class InlinedAPI(self: Inlined) {
    def call(implicit ctx: Context): Option[Tree /* Term | TypeTree */] = kernel.Inlined_call(self)
    def bindings(implicit ctx: Context): List[Definition] = kernel.Inlined_bindings(self)
    def body(implicit ctx: Context): Term = kernel.Inlined_body(self)
  }

  object IsSelectOuter {
    /** Matches any SelectOuter and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[SelectOuter] = kernel.matchSelectOuter(tree)
  }

  object SelectOuter {

    def apply(qualifier: Term, name: String, levels: Int)(implicit ctx: Context): SelectOuter =
      kernel.SelectOuter_apply(qualifier, name, levels)

    def copy(original: Tree)(qualifier: Term, name: String, levels: Int)(implicit ctx: Context): SelectOuter =
      kernel.SelectOuter_copy(original)(qualifier, name, levels)

    def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Int, Type)] = // TODO homogenize order of parameters
      kernel.matchSelectOuter(tree).map(x => (x.qualifier, x.level, x.tpe))

  }

  implicit class SelectOuterAPI(self: SelectOuter) {
    def qualifier(implicit ctx: Context): Term = kernel.SelectOuter_qualifier(self)
    def level(implicit ctx: Context): Int = kernel.SelectOuter_level(self)
    def tpe(implicit ctx: Context): Type = kernel.SelectOuter_tpe(self)
  }

  object IsWhile {
    /** Matches any While and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[While] = kernel.matchWhile(tree)
  }

  object While {

    /** Creates a while loop `while (<cond>) <body>` and returns (<cond>, <body>) */
    def apply(cond: Term, body: Term)(implicit ctx: Context): While =
      kernel.While_apply(cond, body)

    def copy(original: Tree)(cond: Term, body: Term)(implicit ctx: Context): While =
      kernel.While_copy(original)(cond, body)

    /** Extractor for while loops. Matches `while (<cond>) <body>` and returns (<cond>, <body>) */
    def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Term)] =
      kernel.matchWhile(tree).map(x => (x.cond, x.body))

  }

  implicit class WhileAPI(self: While) {
    def cond(implicit ctx: Context): Term = kernel.While_cond(self)
    def body(implicit ctx: Context): Term = kernel.While_body(self)
  }

  // ----- TypeTrees ------------------------------------------------

  implicit class TypeTreeAPI(self: TypeTree) {
    /** Position in the source code */
    def pos(implicit ctx: Context): Position = kernel.TypeTree_pos(self)

    /** Type of this type tree */
    def tpe(implicit ctx: Context): Type = kernel.TypeTree_tpe(self)

    /** Symbol of this type tree */
    def symbol(implicit ctx: Context): Symbol = kernel.TypeTree_symbol(self)
  }

  object IsTypeTree {
    def unapply(tpt: Tree)(implicit ctx: Context): Option[TypeTree] =
      kernel.matchTypeTree(tpt)
  }

  object IsInferred {
    /** Matches any Inferred and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Inferred] =
      kernel.matchInferred(tree)
  }

  /** TypeTree containing an inferred type */
  object Inferred {
    def apply(tpe: Type)(implicit ctx: Context): Inferred =
      kernel.Inferred_apply(tpe)
    /** Matches a TypeTree containing an inferred type */
    def unapply(tree: Tree)(implicit ctx: Context): Boolean =
      kernel.matchInferred(tree).isDefined
  }

  object IsTypeIdent {
    /** Matches any TypeIdent and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[TypeIdent] =
      kernel.matchTypeIdent(tree)
  }

  implicit class TypeIdentAPI(self: TypeIdent) {
    def name(implicit ctx: Context): String = kernel.TypeIdent_name(self)
  }

  object TypeIdent {
    // TODO def apply(name: String)(implicit ctx: Context): TypeIdent
    def copy(original: TypeIdent)(name: String)(implicit ctx: Context): TypeIdent =
      kernel.TypeIdent_copy(original)(name)
    def unapply(tree: Tree)(implicit ctx: Context): Option[String] =
      kernel.matchTypeIdent(tree).map(_.name)
  }

  object IsTypeSelect {
    /** Matches any TypeSelect and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[TypeSelect] =
      kernel.matchTypeSelect(tree)
  }

  object TypeSelect {
    def apply(qualifier: Term, name: String)(implicit ctx: Context): TypeSelect =
      kernel.TypeSelect_apply(qualifier, name)
    def copy(original: TypeSelect)(qualifier: Term, name: String)(implicit ctx: Context): TypeSelect =
      kernel.TypeSelect_copy(original)(qualifier, name)
    def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, String)] =
      kernel.matchTypeSelect(tree).map(x => (x.qualifier, x.name))
  }

  implicit class TypeSelectAPI(self: TypeSelect) {
    def qualifier(implicit ctx: Context): Term = kernel.TypeSelect_qualifier(self)
    def name(implicit ctx: Context): String = kernel.TypeSelect_name(self)
  }

  object IsProjection {
    /** Matches any Projection and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Projection] =
      kernel.matchProjection(tree)
  }

  object Projection {
    // TODO def apply(qualifier: TypeTree, name: String)(implicit ctx: Context): Project
    def copy(original: Projection)(qualifier: TypeTree, name: String)(implicit ctx: Context): Projection =
      kernel.Projection_copy(original)(qualifier, name)
    def unapply(tree: Tree)(implicit ctx: Context): Option[(TypeTree, String)] =
      kernel.matchProjection(tree).map(x => (x.qualifier, x.name))
  }

  implicit class ProjectionAPI(self: Projection) {
    def qualifier(implicit ctx: Context): TypeTree = kernel.Projection_qualifier(self)
    def name(implicit ctx: Context): String = kernel.Projection_name(self)
  }

  object IsSingleton {
    /** Matches any Singleton and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Singleton] =
      kernel.matchSingleton(tree)
  }

  object Singleton {
    def apply(ref: Term)(implicit ctx: Context): Singleton =
      kernel.Singleton_apply(ref)
    def copy(original: Singleton)(ref: Term)(implicit ctx: Context): Singleton =
      kernel.Singleton_copy(original)(ref)
    def unapply(tree: Tree)(implicit ctx: Context): Option[Term] =
      kernel.matchSingleton(tree).map(_.ref)
  }

  implicit class SingletonAPI(self: Singleton) {
    def ref(implicit ctx: Context): Term = kernel.Singleton_ref(self)
  }

  object IsRefined {
    /** Matches any Refined and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Refined] =
      kernel.matchRefined(tree)
  }

  object Refined {
    // TODO def apply(tpt: TypeTree, refinements: List[Definition])(implicit ctx: Context): Refined
    def copy(original: Refined)(tpt: TypeTree, refinements: List[Definition])(implicit ctx: Context): Refined =
      kernel.Refined_copy(original)(tpt, refinements)
    def unapply(tree: Tree)(implicit ctx: Context): Option[(TypeTree, List[Definition])] =
      kernel.matchRefined(tree).map(x => (x.tpt, x.refinements))
  }

  implicit class RefinedAPI(self: Refined) {
    def tpt(implicit ctx: Context): TypeTree = kernel.Refined_tpt(self)
    def refinements(implicit ctx: Context): List[Definition] = kernel.Refined_refinements(self)
  }

  object IsApplied {
    /** Matches any Applied and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Applied] =
      kernel.matchApplied(tree)
  }

  object Applied {
    def apply(tpt: TypeTree, args: List[Tree /*TypeTree | TypeBoundsTree*/])(implicit ctx: Context): Applied =
      kernel.Applied_apply(tpt, args)
    def copy(original: Applied)(tpt: TypeTree, args: List[Tree /*TypeTree | TypeBoundsTree*/])(implicit ctx: Context): Applied =
      kernel.Applied_copy(original)(tpt, args)
    def unapply(tree: Tree)(implicit ctx: Context): Option[(TypeTree, List[Tree /*TypeTree | TypeBoundsTree*/])] =
      kernel.matchApplied(tree).map(x => (x.tpt, x.args))
  }

  implicit class AppliedAPI(self: Applied) {
    def tpt(implicit ctx: Context): TypeTree = kernel.Applied_tpt(self)
    def args(implicit ctx: Context): List[Tree /*TypeTree | TypeBoundsTree*/] = kernel.Applied_args(self)
  }

  object IsAnnotated {
    /** Matches any Annotated and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Annotated] =
      kernel.matchAnnotated(tree)
  }

  object Annotated {
    def apply(arg: TypeTree, annotation: Term)(implicit ctx: Context): Annotated =
      kernel.Annotated_apply(arg, annotation)
    def copy(original: Annotated)(arg: TypeTree, annotation: Term)(implicit ctx: Context): Annotated =
      kernel.Annotated_copy(original)(arg, annotation)
    def unapply(tree: Tree)(implicit ctx: Context): Option[(TypeTree, Term)] =
      kernel.matchAnnotated(tree).map(x => (x.arg, x.annotation))
  }

  implicit class AnnotatedAPI(self: Annotated) {
    def arg(implicit ctx: Context): TypeTree = kernel.Annotated_arg(self)
    def annotation(implicit ctx: Context): Term = kernel.Annotated_annotation(self)
  }

  object IsMatchTypeTree {
    /** Matches any MatchTypeTree and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[MatchTypeTree] =
      kernel.matchMatchTypeTree(tree)
  }

  object MatchTypeTree {
    def apply(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef])(implicit ctx: Context): MatchTypeTree =
      kernel.MatchTypeTree_apply(bound, selector, cases)
    def copy(original: MatchTypeTree)(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef])(implicit ctx: Context): MatchTypeTree =
      kernel.MatchTypeTree_copy(original)(bound, selector, cases)
    def unapply(tree: Tree)(implicit ctx: Context): Option[(Option[TypeTree], TypeTree, List[TypeCaseDef])] =
      kernel.matchMatchTypeTree(tree).map(x => (x.bound, x.selector, x.cases))
  }

  implicit class MatchTypeTreeAPI(self: MatchTypeTree) {
    def bound(implicit ctx: Context): Option[TypeTree] = kernel.MatchTypeTree_bound(self)
    def selector(implicit ctx: Context): TypeTree = kernel.MatchTypeTree_selector(self)
    def cases(implicit ctx: Context): List[TypeCaseDef] = kernel.MatchTypeTree_cases(self)
  }

  object IsByName {
    /** Matches any ByName and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[ByName] =
      kernel.matchByName(tree)
  }

  object ByName {
    def apply(result: TypeTree)(implicit ctx: Context): ByName =
      kernel.ByName_apply(result)
    def copy(original: ByName)(result: TypeTree)(implicit ctx: Context): ByName =
      kernel.ByName_copy(original)(result)
    def unapply(tree: Tree)(implicit ctx: Context): Option[TypeTree] =
      kernel.matchByName(tree).map(_.result)
  }

  implicit class ByNameAPI(self: ByName) {
    def result(implicit ctx: Context): TypeTree = kernel.ByName_result(self)
  }

  object IsLambdaTypeTree {
    /** Matches any LambdaTypeTree and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[LambdaTypeTree] =
      kernel.matchLambdaTypeTree(tree)
  }

  object LambdaTypeTree {
    def apply(tparams: List[TypeDef], body: Tree /*TypeTree | TypeBoundsTree*/)(implicit ctx: Context): LambdaTypeTree =
      kernel.Lambdaapply(tparams, body)
    def copy(original: LambdaTypeTree)(tparams: List[TypeDef], body: Tree /*TypeTree | TypeBoundsTree*/)(implicit ctx: Context): LambdaTypeTree =
      kernel.Lambdacopy(original)(tparams, body)
    def unapply(tree: Tree)(implicit ctx: Context): Option[(List[TypeDef], Tree /*TypeTree | TypeBoundsTree*/)] =
      kernel.matchLambdaTypeTree(tree).map(x => (x.tparams, x.body))
  }

  implicit class LambdaTypeTreeAPI(self: LambdaTypeTree) {
    def tparams(implicit ctx: Context): List[TypeDef] = kernel.Lambdatparams(self)
    def body(implicit ctx: Context): Tree /*TypeTree | TypeBoundsTree*/ = kernel.Lambdabody(self)
  }

  object IsTypeBind {
    /** Matches any TypeBind and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[TypeBind] =
      kernel.matchTypeBind(tree)
  }

  object TypeBind {
    // TODO def apply(name: String, tree: Tree)(implicit ctx: Context): TypeBind
    def copy(original: TypeBind)(name: String, tpt: Tree /*TypeTree | TypeBoundsTree*/)(implicit ctx: Context): TypeBind =
      kernel.TypeBind_copy(original)(name, tpt)
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, Tree /*TypeTree | TypeBoundsTree*/)] =
      kernel.matchTypeBind(tree).map(x => (x.name, x.body))
  }

  implicit class TypeBindAPI(self: TypeBind) {
    def name(implicit ctx: Context): String = kernel.TypeBind_name(self)
    def body(implicit ctx: Context): Tree /*TypeTree | TypeBoundsTree*/ = kernel.TypeBind_body(self)
  }

  object IsTypeBlock {
    /** Matches any TypeBlock and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[TypeBlock] =
      kernel.matchTypeBlock(tree)
  }

  object TypeBlock {
    def apply(aliases: List[TypeDef], tpt: TypeTree)(implicit ctx: Context): TypeBlock =
      kernel.TypeBlock_apply(aliases, tpt)
    def copy(original: TypeBlock)(aliases: List[TypeDef], tpt: TypeTree)(implicit ctx: Context): TypeBlock =
      kernel.TypeBlock_copy(original)(aliases, tpt)
    def unapply(tree: Tree)(implicit ctx: Context): Option[(List[TypeDef], TypeTree)] =
      kernel.matchTypeBlock(tree).map(x => (x.aliases, x.tpt))
  }

  implicit class TypeBlockAPI(self: TypeBlock) {
    def aliases(implicit ctx: Context): List[TypeDef] = kernel.TypeBlock_aliases(self)
    def tpt(implicit ctx: Context): TypeTree = kernel.TypeBlock_tpt(self)
  }

  // ----- TypeBoundsTrees ------------------------------------------------

  implicit class TypeBoundsTreeAPI(self: TypeBoundsTree) {
    def tpe(implicit ctx: Context): TypeBounds = kernel.TypeBoundsTree_tpe(self)
    def low(implicit ctx: Context): TypeTree = kernel.TypeBoundsTree_low(self)
    def hi(implicit ctx: Context): TypeTree = kernel.TypeBoundsTree_hi(self)
  }

  object IsTypeBoundsTree {
    def unapply(tree: Tree)(implicit ctx: Context): Option[TypeBoundsTree] =
      kernel.matchTypeBoundsTree(tree)
  }

  object TypeBoundsTree {
    def unapply(tree: Tree)(implicit ctx: Context): Option[(TypeTree, TypeTree)] =
      kernel.matchTypeBoundsTree(tree).map(x => (x.low, x.hi))
  }

  implicit class WildcardTypeTreeAPI(self: WildcardTypeTree) {
    def tpe(implicit ctx: Context): TypeOrBounds = kernel.WildcardTypeTree_tpe(self)
  }

  object IsWildcardTypeTree {
    def unapply(tree: Tree)(implicit ctx: Context): Option[WildcardTypeTree] =
      kernel.matchWildcardTypeTree(tree)
  }

  /** TypeBoundsTree containing wildcard type bounds */
  object WildcardTypeTree {
    /** Matches a TypeBoundsTree containing wildcard type bounds */
    def unapply(tree: Tree)(implicit ctx: Context): Boolean =
      kernel.matchWildcardTypeTree(tree).isDefined
  }

  // ----- CaseDefs ------------------------------------------------

  implicit class CaseDefAPI(caseDef: CaseDef) {
    def pattern(implicit ctx: Context): Pattern = kernel.CaseDef_pattern(caseDef)
    def guard(implicit ctx: Context): Option[Term] = kernel.CaseDef_guard(caseDef)
    def rhs(implicit ctx: Context): Term = kernel.CaseDef_rhs(caseDef)
  }

  object IsCaseDef {
    def unapply(self: Tree)(implicit ctx: Context): Option[CaseDef] =
      kernel.matchCaseDef(self)
  }

  object CaseDef {
    def apply(pattern: Pattern, guard: Option[Term], rhs: Term)(implicit ctx: Context): CaseDef =
      kernel.CaseDef_module_apply(pattern, guard, rhs)

    def copy(original: CaseDef)(pattern: Pattern, guard: Option[Term], rhs: Term)(implicit ctx: Context): CaseDef =
      kernel.CaseDef_module_copy(original)(pattern, guard, rhs)

    def unapply(tree: Tree)(implicit ctx: Context): Option[(Pattern, Option[Term], Term)] =
      kernel.matchCaseDef(tree).map( x => (x.pattern, x.guard, x.rhs))
  }

  implicit class TypeCaseDefAPI(caseDef: TypeCaseDef) {
    def pattern(implicit ctx: Context): TypeTree = kernel.TypeCaseDef_pattern(caseDef)
    def rhs(implicit ctx: Context): TypeTree = kernel.TypeCaseDef_rhs(caseDef)
  }

  object IsTypeCaseDef {
    def unapply(self: Tree)(implicit ctx: Context): Option[TypeCaseDef] =
      kernel.matchTypeCaseDef(self)
  }

  object TypeCaseDef {
    def apply(pattern: TypeTree, rhs: TypeTree)(implicit ctx: Context): TypeCaseDef =
      kernel.TypeCaseDef_module_apply(pattern, rhs)

    def copy(original: TypeCaseDef)(pattern: TypeTree, rhs: TypeTree)(implicit ctx: Context): TypeCaseDef =
      kernel.TypeCaseDef_module_copy(original)(pattern, rhs)

    def unapply(tree: Tree)(implicit ctx: Context): Option[(TypeTree, TypeTree)] =
      kernel.matchTypeCaseDef(tree).map( x => (x.pattern, x.rhs))
  }
}
