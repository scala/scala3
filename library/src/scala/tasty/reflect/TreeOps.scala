package scala.tasty
package reflect

trait TreeOps extends Core {

  // ----- Tree -----------------------------------------------------

  given (self: Tree) {
    /** Position in the source code */
    def pos(given ctx: Context): Position = internal.Tree_pos(self)

    def symbol(given ctx: Context): Symbol = internal.Tree_symbol(self)
  }

  object IsPackageClause {
    def unapply(tree: Tree)(given ctx: Context): Option[PackageClause] =
      internal.matchPackageClause(tree)
  }

  object PackageClause {
    def apply(pid: Ref, stats: List[Tree])(given ctx: Context): PackageClause =
      internal.PackageClause_apply(pid, stats)
    def copy(original: Tree)(pid: Ref, stats: List[Tree])(given ctx: Context): PackageClause =
      internal.PackageClause_copy(original)(pid, stats)
    def unapply(tree: Tree)(given ctx: Context): Option[(Ref, List[Tree])] =
      internal.matchPackageClause(tree).map(x => (x.pid, x.stats))
  }

  given (self: PackageClause) {
    def pid(given ctx: Context): Ref = internal.PackageClause_pid(self)
    def stats(given ctx: Context): List[Tree] = internal.PackageClause_stats(self)
  }

  object IsImport {
    def unapply(tree: Tree)(given ctx: Context): Option[Import] =
      internal.matchImport(tree)
  }

  object Import {
    def apply(expr: Term, selectors: List[ImportSelector])(given ctx: Context): Import =
      internal.Import_apply(expr, selectors)
    def copy(original: Tree)(expr: Term, selectors: List[ImportSelector])(given ctx: Context): Import =
      internal.Import_copy(original)(expr, selectors)
    def unapply(tree: Tree)(given ctx: Context): Option[(Term, List[ImportSelector])] =
      internal.matchImport(tree).map(x => (x.expr, x.selectors))
  }

  given (self: Import)  {
    def expr(given ctx: Context): Term = internal.Import_expr(self)
    def selectors(given ctx: Context): List[ImportSelector] =
      internal.Import_selectors(self)
  }

  object IsStatement {
    /** Matches any Statement and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[Statement] = internal.matchStatement(tree)
  }

  // ----- Definitions ----------------------------------------------

  object IsDefinition {
    def unapply(tree: Tree)(given ctx: Context): Option[Definition] = internal.matchDefinition(tree)
  }

  given (self: Definition) {
    def name(given ctx: Context): String = internal.Definition_name(self)
  }

  // ClassDef

  object IsClassDef {
    def unapply(tree: Tree)(given ctx: Context): Option[ClassDef] = internal.matchClassDef(tree)
  }

  object ClassDef {
    // TODO def apply(name: String, constr: DefDef, parents: List[TermOrTypeTree], selfOpt: Option[ValDef], body: List[Statement])(given ctx: Context): ClassDef
    def copy(original: Tree)(name: String, constr: DefDef, parents: List[Tree /* Term | TypeTree */], derived: List[TypeTree], selfOpt: Option[ValDef], body: List[Statement])(given ctx: Context): ClassDef =
      internal.ClassDef_copy(original)(name, constr, parents, derived, selfOpt, body)
    def unapply(tree: Tree)(given ctx: Context): Option[(String, DefDef, List[Tree /* Term | TypeTree */], List[TypeTree], Option[ValDef], List[Statement])] =
      internal.matchClassDef(tree).map(x => (x.name, x.constructor, x.parents, x.derived, x.self, x.body))
  }

  given (self: ClassDef) {
    def constructor(given ctx: Context): DefDef = internal.ClassDef_constructor(self)
    def parents(given ctx: Context): List[Tree /* Term | TypeTree */] = internal.ClassDef_parents(self)
    def derived(given ctx: Context): List[TypeTree] = internal.ClassDef_derived(self)
    def self(given ctx: Context): Option[ValDef] = internal.ClassDef_self(self)
    def body(given ctx: Context): List[Statement] = internal.ClassDef_body(self)
  }

  // DefDef

  object IsDefDef {
    def unapply(tree: Tree)(given ctx: Context): Option[DefDef] = internal.matchDefDef(tree)
  }

  object DefDef {
    def apply(symbol: Symbol, rhsFn: List[Type] => List[List[Term]] => Option[Term])(given ctx: Context): DefDef =
      internal.DefDef_apply(symbol, rhsFn)
    def copy(original: Tree)(name: String, typeParams: List[TypeDef], paramss: List[List[ValDef]], tpt: TypeTree, rhs: Option[Term])(given ctx: Context): DefDef =
      internal.DefDef_copy(original)(name, typeParams, paramss, tpt, rhs)
    def unapply(tree: Tree)(given ctx: Context): Option[(String, List[TypeDef], List[List[ValDef]], TypeTree, Option[Term])] =
      internal.matchDefDef(tree).map(x => (x.name, x.typeParams, x.paramss, x.returnTpt, x.rhs))
  }

  given (self: DefDef) {
    def typeParams(given ctx: Context): List[TypeDef] = internal.DefDef_typeParams(self)
    def paramss(given ctx: Context): List[List[ValDef]] = internal.DefDef_paramss(self)
    def returnTpt(given ctx: Context): TypeTree = internal.DefDef_returnTpt(self) // TODO rename to tpt
    def rhs(given ctx: Context): Option[Term] = internal.DefDef_rhs(self)
  }

  // ValDef

  object IsValDef {
    def unapply(tree: Tree)(given ctx: Context): Option[ValDef] = internal.matchValDef(tree)
  }

  object ValDef {
    def apply(symbol: Symbol, rhs: Option[Term])(given ctx: Context): ValDef =
      internal.ValDef_apply(symbol, rhs)
    def copy(original: Tree)(name: String, tpt: TypeTree, rhs: Option[Term])(given ctx: Context): ValDef =
      internal.ValDef_copy(original)(name, tpt, rhs)
    def unapply(tree: Tree)(given ctx: Context): Option[(String, TypeTree, Option[Term])] =
      internal.matchValDef(tree).map(x => (x.name, x.tpt, x.rhs))
  }

  given (self: ValDef) {
    def tpt(given ctx: Context): TypeTree = internal.ValDef_tpt(self)
    def rhs(given ctx: Context): Option[Term] = internal.ValDef_rhs(self)
  }

  // TypeDef

  object IsTypeDef {
    def unapply(tree: Tree)(given ctx: Context): Option[TypeDef] = internal.matchTypeDef(tree)
  }

  object TypeDef {
    def apply(symbol: Symbol)(given ctx: Context): TypeDef =
      internal.TypeDef_apply(symbol)
    def copy(original: Tree)(name: String, rhs: Tree /*TypeTree | TypeBoundsTree*/)(given ctx: Context): TypeDef =
      internal.TypeDef_copy(original)(name, rhs)
    def unapply(tree: Tree)(given ctx: Context): Option[(String, Tree /*TypeTree | TypeBoundsTree*/ /* TypeTree | TypeBoundsTree */)] =
      internal.matchTypeDef(tree).map(x => (x.name, x.rhs))
  }

  given (self: TypeDef) {
    def rhs(given ctx: Context): Tree /*TypeTree | TypeBoundsTree*/ = internal.TypeDef_rhs(self)
  }

  // PackageDef

  object IsPackageDef {
    def unapply(tree: Tree)(given ctx: Context): Option[PackageDef] =
      internal.matchPackageDef(tree)
  }

  given (self: PackageDef) {
    def owner(given ctx: Context): PackageDef = internal.PackageDef_owner(self)
    def members(given ctx: Context): List[Statement] = internal.PackageDef_members(self)
  }

  object PackageDef {
    def unapply(tree: Tree)(given ctx: Context): Option[(String, PackageDef)] =
      internal.matchPackageDef(tree).map(x => (x.name, x.owner))
  }

  // ----- Terms ----------------------------------------------------

  given (self: Term) {
    def tpe(given ctx: Context): Type = internal.Term_tpe(self)
    def underlyingArgument(given ctx: Context): Term = internal.Term_underlyingArgument(self)
    def underlying(given ctx: Context): Term = internal.Term_underlying(self)
    def etaExpand(given ctx: Context): Term = internal.Term_etaExpand(self)

    /** A unary apply node with given argument: `tree(arg)` */
    def appliedTo(arg: Term)(given ctx: Context): Term =
      self.appliedToArgs(arg :: Nil)

    /** An apply node with given arguments: `tree(arg, args0, ..., argsN)` */
    def appliedTo(arg: Term, args: Term*)(given ctx: Context): Term =
      self.appliedToArgs(arg :: args.toList)

    /** An apply node with given argument list `tree(args(0), ..., args(args.length - 1))` */
    def appliedToArgs(args: List[Term])(given ctx: Context): Apply =
      Apply(self, args)

    /** The current tree applied to given argument lists:
     *  `tree (argss(0)) ... (argss(argss.length -1))`
     */
    def appliedToArgss(argss: List[List[Term]])(given ctx: Context): Term =
      argss.foldLeft(self: Term)(Apply(_, _))

    /** The current tree applied to (): `tree()` */
    def appliedToNone(given ctx: Context): Apply =
      self.appliedToArgs(Nil)

    /** The current tree applied to given type argument: `tree[targ]` */
    def appliedToType(targ: Type)(given ctx: Context): Term =
      self.appliedToTypes(targ :: Nil)

    /** The current tree applied to given type arguments: `tree[targ0, ..., targN]` */
    def appliedToTypes(targs: List[Type])(given ctx: Context): Term =
      self.appliedToTypeTrees(targs map (Inferred(_)))

    /** The current tree applied to given type argument list: `tree[targs(0), ..., targs(targs.length - 1)]` */
    def appliedToTypeTrees(targs: List[TypeTree])(given ctx: Context): Term =
      if (targs.isEmpty) self else TypeApply(self, targs)

    /** A select node that selects the given symbol.
     */
    def select(sym: Symbol)(given ctx: Context): Select = Select(self, sym)
  }

  object IsTerm {
    /** Matches any term */
    def unapply(tree: Tree)(given ctx: Context): Option[Term] =
      internal.matchTerm(tree)
  }

  object IsRef {
    /** Matches any Ref and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[Ref] = internal.matchRef(tree)
  }

  object Ref {

    /** Create a reference tree */
    def apply(sym: Symbol)(given ctx: Context): Ref =
      internal.Ref_apply(sym)

    // TODO def copy(original: Tree)(name: String)(given ctx: Context): Ref

  }

  object IsIdent {
    /** Matches any Ident and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[Ident] = internal.matchIdent(tree)
  }

  given (self: Ident) {
    def name(given ctx: Context): String = internal.Ident_name(self)
  }

  /** Scala term identifier */
  object Ident {
    def apply(tmref: TermRef)(given ctx: Context): Term =
      internal.Ident_apply(tmref)

    def copy(original: Tree)(name: String)(given ctx: Context): Ident =
      internal.Ident_copy(original)(name)

    /** Matches a term identifier and returns its name */
    def unapply(tree: Tree)(given ctx: Context): Option[String] =
      internal.matchIdent(tree).map(_.name)
  }

  object IsSelect {
    /** Matches any Select and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[Select] = internal.matchSelect(tree)
  }

  /** Scala term selection */
  object Select {
    /** Select a term member by symbol */
    def apply(qualifier: Term, symbol: Symbol)(given ctx: Context): Select =
      internal.Select_apply(qualifier, symbol)

    /** Select a field or a non-overloaded method by name
     *
     *  @note The method will produce an assertion error if the selected
     *        method is overloaded. The method `overloaded` should be used
     *        in that case.
     */
    def unique(qualifier: Term, name: String)(given ctx: Context): Select =
      internal.Select_unique(qualifier, name)

    // TODO rename, this returns an Apply and not a Select
    /** Call an overloaded method with the given type and term parameters */
    def overloaded(qualifier: Term, name: String, targs: List[Type], args: List[Term])(given ctx: Context): Apply =
      internal.Select_overloaded(qualifier, name, targs, args)

    def copy(original: Tree)(qualifier: Term, name: String)(given ctx: Context): Select =
      internal.Select_copy(original)(qualifier, name)

    /** Matches `<qualifier: Term>.<name: String>` */
    def unapply(tree: Tree)(given ctx: Context): Option[(Term, String)] =
      internal.matchSelect(tree).map(x => (x.qualifier, x.name))
  }

  given (self: Select) {
    def qualifier(given ctx: Context): Term = internal.Select_qualifier(self)
    def name(given ctx: Context): String = internal.Select_name(self)
    def signature(given ctx: Context): Option[Signature] = internal.Select_signature(self)
  }

  object IsLiteral {
    /** Matches any Literal and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[Literal] = internal.matchLiteral(tree)
  }

  /** Scala literal constant */
  object Literal {

    /** Create a literal constant */
    def apply(constant: Constant)(given ctx: Context): Literal =
      internal.Literal_apply(constant)

    def copy(original: Tree)(constant: Constant)(given ctx: Context): Literal =
      internal.Literal_copy(original)(constant)

    /** Matches a literal constant */
    def unapply(tree: Tree)(given ctx: Context): Option[Constant] =
      internal.matchLiteral(tree).map(_.constant)
  }

  given (self:  Literal) {
    def constant(given ctx: Context): Constant = internal.Literal_constant(self)
  }

  object IsThis {
    /** Matches any This and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[This] = internal.matchThis(tree)
  }

  /** Scala `this` or `this[id]` */
  object This {

    /** Create a `this[<id: Id]>` */
    def apply(cls: Symbol)(given ctx: Context): This =
      internal.This_apply(cls)

    def copy(original: Tree)(qual: Option[Id])(given ctx: Context): This =
      internal.This_copy(original)(qual)

    /** Matches `this[<id: Option[Id]>` */
    def unapply(tree: Tree)(given ctx: Context): Option[Option[Id]] =
      internal.matchThis(tree).map(_.id)

  }

  given (self:  This) {
    def id(given ctx: Context): Option[Id] = internal.This_id(self)
  }

  object IsNew {
    /** Matches any New and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[New] = internal.matchNew(tree)
  }

  /** Scala `new` */
  object New {

    /** Create a `new <tpt: TypeTree>` */
    def apply(tpt: TypeTree)(given ctx: Context): New =
      internal.New_apply(tpt)

    def copy(original: Tree)(tpt: TypeTree)(given ctx: Context): New =
      internal.New_copy(original)(tpt)

    /** Matches a `new <tpt: TypeTree>` */
    def unapply(tree: Tree)(given ctx: Context): Option[TypeTree] =
      internal.matchNew(tree).map(_.tpt)
  }

  given (self: New) {
    def tpt(given ctx: Context): TypeTree = internal.New_tpt(self)
  }

  object IsNamedArg {
    /** Matches any NamedArg and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[NamedArg] = internal.matchNamedArg(tree)
  }

  /** Scala named argument `x = y` in argument position */
  object NamedArg {

    /** Create a named argument `<name: String> = <value: Term>` */
    def apply(name: String, arg: Term)(given ctx: Context): NamedArg =
      internal.NamedArg_apply(name, arg)

    def copy(original: Tree)(name: String, arg: Term)(given ctx: Context): NamedArg =
      internal.NamedArg_copy(original)(name, arg)

    /** Matches a named argument `<name: String> = <value: Term>` */
    def unapply(tree: Tree)(given ctx: Context): Option[(String, Term)] =
      internal.matchNamedArg(tree).map(x => (x.name, x.value))

  }

  given (self: NamedArg) {
    def name(given ctx: Context): String = internal.NamedArg_name(self)
    def value(given ctx: Context): Term = internal.NamedArg_value(self)
  }

  object IsApply {
    /** Matches any Apply and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[Apply] = internal.matchApply(tree)
  }

  /** Scala parameter application */
  object Apply {

    /** Create a function application `<fun: Term>(<args: List[Term]>)` */
    def apply(fun: Term, args: List[Term])(given ctx: Context): Apply =
      internal.Apply_apply(fun, args)

    def copy(original: Tree)(fun: Term, args: List[Term])(given ctx: Context): Apply =
      internal.Apply_copy(original)(fun, args)

    /** Matches a function application `<fun: Term>(<args: List[Term]>)` */
    def unapply(tree: Tree)(given ctx: Context): Option[(Term, List[Term])] =
      internal.matchApply(tree).map(x => (x.fun, x.args))
  }

  given (self: Apply) {
    def fun(given ctx: Context): Term = internal.Apply_fun(self)
    def args(given ctx: Context): List[Term] = internal.Apply_args(self)
  }

  object IsTypeApply {
    /** Matches any TypeApply and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[TypeApply] =
      internal.matchTypeApply(tree)
  }

  /** Scala type parameter application */
  object TypeApply {

    /** Create a function type application `<fun: Term>[<args: List[TypeTree]>]` */
    def apply(fun: Term, args: List[TypeTree])(given ctx: Context): TypeApply =
      internal.TypeApply_apply(fun, args)

    def copy(original: Tree)(fun: Term, args: List[TypeTree])(given ctx: Context): TypeApply =
      internal.TypeApply_copy(original)(fun, args)

    /** Matches a function type application `<fun: Term>[<args: List[TypeTree]>]` */
    def unapply(tree: Tree)(given ctx: Context): Option[(Term, List[TypeTree])] =
      internal.matchTypeApply(tree).map(x => (x.fun, x.args))

  }

  given (self: TypeApply) {
    def fun(given ctx: Context): Term = internal.TypeApply_fun(self)
    def args(given ctx: Context): List[TypeTree] = internal.TypeApply_args(self)
  }

  object IsSuper {
    /** Matches any Super and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[Super] = internal.matchSuper(tree)
  }

  /** Scala `x.super` or `x.super[id]` */
  object Super {

    /** Creates a `<qualifier: Term>.super[<id: Option[Id]>` */
    def apply(qual: Term, mix: Option[Id])(given ctx: Context): Super =
      internal.Super_apply(qual, mix)

    def copy(original: Tree)(qual: Term, mix: Option[Id])(given ctx: Context): Super =
      internal.Super_copy(original)(qual, mix)

    /** Matches a `<qualifier: Term>.super[<id: Option[Id]>` */
    def unapply(tree: Tree)(given ctx: Context): Option[(Term, Option[Id])] =
      internal.matchSuper(tree).map(x => (x.qualifier, x.id))
  }

  given (self: Super) {
    def qualifier(given ctx: Context): Term = internal.Super_qualifier(self)
    def id(given ctx: Context): Option[Id] = internal.Super_id(self)
  }

  object IsTyped {
    /** Matches any Typed and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[Typed] = internal.matchTyped(tree)
  }

  /** Scala ascription `x: T` */
  object Typed {

    /** Create a type ascription `<x: Term>: <tpt: TypeTree>` */
    def apply(expr: Term, tpt: TypeTree)(given ctx: Context): Typed =
      internal.Typed_apply(expr, tpt)

    def copy(original: Tree)(expr: Term, tpt: TypeTree)(given ctx: Context): Typed =
      internal.Typed_copy(original)(expr, tpt)

    /** Matches `<expr: Term>: <tpt: TypeTree>` */
    def unapply(tree: Tree)(given ctx: Context): Option[(Term, TypeTree)] =
      internal.matchTyped(tree).map(x => (x.expr, x.tpt))

  }

  given (self: Typed) {
    def expr(given ctx: Context): Term = internal.Typed_expr(self)
    def tpt(given ctx: Context): TypeTree = internal.Typed_tpt(self)
  }

  object IsAssign {
    /** Matches any Assign and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[Assign] = internal.matchAssign(tree)
  }

  /** Scala assign `x = y` */
  object Assign {

    /** Create an assignment `<lhs: Term> = <rhs: Term>` */
    def apply(lhs: Term, rhs: Term)(given ctx: Context): Assign =
      internal.Assign_apply(lhs, rhs)

    def copy(original: Tree)(lhs: Term, rhs: Term)(given ctx: Context): Assign =
      internal.Assign_copy(original)(lhs, rhs)

    /** Matches an assignment `<lhs: Term> = <rhs: Term>` */
    def unapply(tree: Tree)(given ctx: Context): Option[(Term, Term)] =
      internal.matchAssign(tree).map(x => (x.lhs, x.rhs))
  }

  given (self: Assign) {
    def lhs(given ctx: Context): Term = internal.Assign_lhs(self)
    def rhs(given ctx: Context): Term = internal.Assign_rhs(self)
  }

  object IsBlock {
    /** Matches any Block and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[Block] = internal.matchBlock(tree)
  }

  /** Scala code block `{ stat0; ...; statN; expr }` term */
  object Block {

    /** Creates a block `{ <statements: List[Statement]>; <expr: Term> }` */
    def apply(stats: List[Statement], expr: Term)(given ctx: Context): Block =
      internal.Block_apply(stats, expr)

    def copy(original: Tree)(stats: List[Statement], expr: Term)(given ctx: Context): Block =
      internal.Block_copy(original)(stats, expr)

    /** Matches a block `{ <statements: List[Statement]>; <expr: Term> }` */
    def unapply(tree: Tree)(given ctx: Context): Option[(List[Statement], Term)] =
      internal.matchBlock(tree).map(x => (x.statements, x.expr))
  }

  given (self: Block) {
    def statements(given ctx: Context): List[Statement] = internal.Block_statements(self)
    def expr(given ctx: Context): Term = internal.Block_expr(self)
  }

  object IsClosure {
    /** Matches any Closure and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[Closure] = internal.matchClosure(tree)
  }

  object Closure {

    def apply(meth: Term, tpt: Option[Type])(given ctx: Context): Closure =
      internal.Closure_apply(meth, tpt)

    def copy(original: Tree)(meth: Tree, tpt: Option[Type])(given ctx: Context): Closure =
      internal.Closure_copy(original)(meth, tpt)

    def unapply(tree: Tree)(given ctx: Context): Option[(Term, Option[Type])] =
      internal.matchClosure(tree).map(x => (x.meth, x.tpeOpt))
  }

  given (self: Closure) {
    def meth(given ctx: Context): Term = internal.Closure_meth(self)
    def tpeOpt(given ctx: Context): Option[Type] = internal.Closure_tpeOpt(self)
  }

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
  object Lambda {
    def unapply(tree: Tree)(given ctx: Context): Option[(List[ValDef], Term)] = tree match {
      case Block((ddef @ DefDef(_, _, params :: Nil, _, Some(body))) :: Nil, Closure(meth, _))
      if ddef.symbol == meth.symbol =>
        Some(params, body)

      case _ => None
    }
  }

  object IsIf {
    /** Matches any If and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[If] = internal.matchIf(tree)
  }

  /** Scala `if`/`else` term */
  object If {

    /** Create an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>` */
    def apply(cond: Term, thenp: Term, elsep: Term)(given ctx: Context): If =
      internal.If_apply(cond, thenp, elsep)

    def copy(original: Tree)(cond: Term, thenp: Term, elsep: Term)(given ctx: Context): If =
      internal.If_copy(original)(cond, thenp, elsep)

    /** Matches an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>` */
    def unapply(tree: Tree)(given ctx: Context): Option[(Term, Term, Term)] =
      internal.matchIf(tree).map(x => (x.cond, x.thenp, x.elsep))

  }

  given (self: If) {
    def cond(given ctx: Context): Term = internal.If_cond(self)
    def thenp(given ctx: Context): Term = internal.If_thenp(self)
    def elsep(given ctx: Context): Term = internal.If_elsep(self)
  }

  object IsMatch {
    /** Matches any Match and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[Match] = internal.matchMatch(tree)
  }

  /** Scala `match` term */
  object Match {

    /** Creates a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }` */
    def apply(selector: Term, cases: List[CaseDef])(given ctx: Context): Match =
      internal.Match_apply(selector, cases)

    def copy(original: Tree)(selector: Term, cases: List[CaseDef])(given ctx: Context): Match =
      internal.Match_copy(original)(selector, cases)

    /** Matches a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }` */
    def unapply(tree: Tree)(given ctx: Context): Option[(Term, List[CaseDef])] =
      internal.matchMatch(tree).map(x => (x.scrutinee, x.cases))

  }

  given (self: Match) {
    def scrutinee(given ctx: Context): Term = internal.Match_scrutinee(self)
    def cases(given ctx: Context): List[CaseDef] = internal.Match_cases(self)
  }

  object IsImplicitMatch {
    /** Matches any ImpliedMatch and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[ImpliedMatch] = internal.matchImplicitMatch(tree)
  }

  /** Scala implicit `match` term */
  object ImpliedMatch {

    /** Creates a pattern match `delegate match { <cases: List[CaseDef]> }` */
    def apply(cases: List[CaseDef])(given ctx: Context): ImpliedMatch =
      internal.ImplicitMatch_apply(cases)

    def copy(original: Tree)(cases: List[CaseDef])(given ctx: Context): ImpliedMatch =
      internal.ImplicitMatch_copy(original)(cases)

    /** Matches a pattern match `delegate match { <cases: List[CaseDef]> }` */
    def unapply(tree: Tree)(given ctx: Context): Option[List[CaseDef]] =
      internal.matchImplicitMatch(tree).map(_.cases)

  }

  given (self: ImpliedMatch) {
    def cases(given ctx: Context): List[CaseDef] = internal.ImplicitMatch_cases(self)
  }

  object IsTry {
    /** Matches any Try and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[Try] = internal.matchTry(tree)
  }

  /** Scala `try`/`catch`/`finally` term */
  object Try {

    /** Create a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>` */
    def apply(expr: Term, cases: List[CaseDef], finalizer: Option[Term])(given ctx: Context): Try =
      internal.Try_apply(expr, cases, finalizer)

    def copy(original: Tree)(expr: Term, cases: List[CaseDef], finalizer: Option[Term])(given ctx: Context): Try =
      internal.Try_copy(original)(expr, cases, finalizer)

    /** Matches a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>` */
    def unapply(tree: Tree)(given ctx: Context): Option[(Term, List[CaseDef], Option[Term])] =
      internal.matchTry(tree).map(x => (x.body, x.cases, x.finalizer))

  }

  given (self: Try) {
    def body(given ctx: Context): Term = internal.Try_body(self)
    def cases(given ctx: Context): List[CaseDef] = internal.Try_cases(self)
    def finalizer(given ctx: Context): Option[Term] = internal.Try_finalizer(self)
  }

  object IsReturn {
    /** Matches any Return and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[Return] = internal.matchReturn(tree)
  }

  /** Scala local `return` */
  object Return {

    /** Creates `return <expr: Term>` */
    def apply(expr: Term)(given ctx: Context): Return =
      internal.Return_apply(expr)

    def copy(original: Tree)(expr: Term)(given ctx: Context): Return =
      internal.Return_copy(original)(expr)

    /** Matches `return <expr: Term>` */
    def unapply(tree: Tree)(given ctx: Context): Option[Term] =
      internal.matchReturn(tree).map(_.expr)

  }

  given (self: Return) {
    def expr(given ctx: Context): Term = internal.Return_expr(self)
  }

  object IsRepeated {
    /** Matches any Repeated and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[Repeated] = internal.matchRepeated(tree)
  }

  object Repeated {

    def apply(elems: List[Term], tpt: TypeTree)(given ctx: Context): Repeated =
      internal.Repeated_apply(elems, tpt)

    def copy(original: Tree)(elems: List[Term], tpt: TypeTree)(given ctx: Context): Repeated =
      internal.Repeated_copy(original)(elems, tpt)

    def unapply(tree: Tree)(given ctx: Context): Option[(List[Term], TypeTree)] =
      internal.matchRepeated(tree).map(x => (x.elems, x.elemtpt))

  }

  given (self: Repeated) {
    def elems(given ctx: Context): List[Term] = internal.Repeated_elems(self)
    def elemtpt(given ctx: Context): TypeTree = internal.Repeated_elemtpt(self)
  }

  object IsInlined {
    /** Matches any Inlined and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[Inlined] = internal.matchInlined(tree)
  }

  object Inlined {

    def apply(call: Option[Tree /* Term | TypeTree */], bindings: List[Definition], expansion: Term)(given ctx: Context): Inlined =
      internal.Inlined_apply(call, bindings, expansion)

    def copy(original: Tree)(call: Option[Tree /* Term | TypeTree */], bindings: List[Definition], expansion: Term)(given ctx: Context): Inlined =
      internal.Inlined_copy(original)(call, bindings, expansion)

    def unapply(tree: Tree)(given ctx: Context): Option[(Option[Tree /* Term | TypeTree */], List[Definition], Term)] =
      internal.matchInlined(tree).map(x => (x.call, x.bindings, x.body))

  }

  given (self: Inlined) {
    def call(given ctx: Context): Option[Tree /* Term | TypeTree */] = internal.Inlined_call(self)
    def bindings(given ctx: Context): List[Definition] = internal.Inlined_bindings(self)
    def body(given ctx: Context): Term = internal.Inlined_body(self)
  }

  object IsSelectOuter {
    /** Matches any SelectOuter and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[SelectOuter] = internal.matchSelectOuter(tree)
  }

  object SelectOuter {

    def apply(qualifier: Term, name: String, levels: Int)(given ctx: Context): SelectOuter =
      internal.SelectOuter_apply(qualifier, name, levels)

    def copy(original: Tree)(qualifier: Term, name: String, levels: Int)(given ctx: Context): SelectOuter =
      internal.SelectOuter_copy(original)(qualifier, name, levels)

    def unapply(tree: Tree)(given ctx: Context): Option[(Term, Int, Type)] = // TODO homogenize order of parameters
      internal.matchSelectOuter(tree).map(x => (x.qualifier, x.level, x.tpe))

  }

  given (self: SelectOuter) {
    def qualifier(given ctx: Context): Term = internal.SelectOuter_qualifier(self)
    def level(given ctx: Context): Int = internal.SelectOuter_level(self)
  }

  object IsWhile {
    /** Matches any While and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[While] = internal.matchWhile(tree)
  }

  object While {

    /** Creates a while loop `while (<cond>) <body>` and returns (<cond>, <body>) */
    def apply(cond: Term, body: Term)(given ctx: Context): While =
      internal.While_apply(cond, body)

    def copy(original: Tree)(cond: Term, body: Term)(given ctx: Context): While =
      internal.While_copy(original)(cond, body)

    /** Extractor for while loops. Matches `while (<cond>) <body>` and returns (<cond>, <body>) */
    def unapply(tree: Tree)(given ctx: Context): Option[(Term, Term)] =
      internal.matchWhile(tree).map(x => (x.cond, x.body))

  }

  given (self: While) {
    def cond(given ctx: Context): Term = internal.While_cond(self)
    def body(given ctx: Context): Term = internal.While_body(self)
  }

  // ----- TypeTrees ------------------------------------------------

  given (self: TypeTree) {
    /** Type of this type tree */
    def tpe(given ctx: Context): Type = internal.TypeTree_tpe(self)
  }

  object IsTypeTree {
    def unapply(tpt: Tree)(given ctx: Context): Option[TypeTree] =
      internal.matchTypeTree(tpt)
  }

  object IsInferred {
    /** Matches any Inferred and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[Inferred] =
      internal.matchInferred(tree)
  }

  /** TypeTree containing an inferred type */
  object Inferred {
    def apply(tpe: Type)(given ctx: Context): Inferred =
      internal.Inferred_apply(tpe)
    /** Matches a TypeTree containing an inferred type */
    def unapply(tree: Tree)(given ctx: Context): Boolean =
      internal.matchInferred(tree).isDefined
  }

  object IsTypeIdent {
    /** Matches any TypeIdent and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[TypeIdent] =
      internal.matchTypeIdent(tree)
  }

  given (self: TypeIdent) {
    def name(given ctx: Context): String = internal.TypeIdent_name(self)
  }

  object TypeIdent {
    // TODO def apply(name: String)(given ctx: Context): TypeIdent
    def copy(original: Tree)(name: String)(given ctx: Context): TypeIdent =
      internal.TypeIdent_copy(original)(name)
    def unapply(tree: Tree)(given ctx: Context): Option[String] =
      internal.matchTypeIdent(tree).map(_.name)
  }

  object IsTypeSelect {
    /** Matches any TypeSelect and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[TypeSelect] =
      internal.matchTypeSelect(tree)
  }

  object TypeSelect {
    def apply(qualifier: Term, name: String)(given ctx: Context): TypeSelect =
      internal.TypeSelect_apply(qualifier, name)
    def copy(original: Tree)(qualifier: Term, name: String)(given ctx: Context): TypeSelect =
      internal.TypeSelect_copy(original)(qualifier, name)
    def unapply(tree: Tree)(given ctx: Context): Option[(Term, String)] =
      internal.matchTypeSelect(tree).map(x => (x.qualifier, x.name))
  }

  given (self: TypeSelect) {
    def qualifier(given ctx: Context): Term = internal.TypeSelect_qualifier(self)
    def name(given ctx: Context): String = internal.TypeSelect_name(self)
  }

  object IsProjection {
    /** Matches any Projection and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[Projection] =
      internal.matchProjection(tree)
  }

  object Projection {
    // TODO def apply(qualifier: TypeTree, name: String)(given ctx: Context): Project
    def copy(original: Tree)(qualifier: TypeTree, name: String)(given ctx: Context): Projection =
      internal.Projection_copy(original)(qualifier, name)
    def unapply(tree: Tree)(given ctx: Context): Option[(TypeTree, String)] =
      internal.matchProjection(tree).map(x => (x.qualifier, x.name))
  }

  given (self: Projection) {
    def qualifier(given ctx: Context): TypeTree = internal.Projection_qualifier(self)
    def name(given ctx: Context): String = internal.Projection_name(self)
  }

  object IsSingleton {
    /** Matches any Singleton and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[Singleton] =
      internal.matchSingleton(tree)
  }

  object Singleton {
    def apply(ref: Term)(given ctx: Context): Singleton =
      internal.Singleton_apply(ref)
    def copy(original: Tree)(ref: Term)(given ctx: Context): Singleton =
      internal.Singleton_copy(original)(ref)
    def unapply(tree: Tree)(given ctx: Context): Option[Term] =
      internal.matchSingleton(tree).map(_.ref)
  }

  given (self: Singleton) {
    def ref(given ctx: Context): Term = internal.Singleton_ref(self)
  }

  object IsRefined {
    /** Matches any Refined and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[Refined] =
      internal.matchRefined(tree)
  }

  object Refined {
    // TODO def apply(tpt: TypeTree, refinements: List[Definition])(given ctx: Context): Refined
    def copy(original: Tree)(tpt: TypeTree, refinements: List[Definition])(given ctx: Context): Refined =
      internal.Refined_copy(original)(tpt, refinements)
    def unapply(tree: Tree)(given ctx: Context): Option[(TypeTree, List[Definition])] =
      internal.matchRefined(tree).map(x => (x.tpt, x.refinements))
  }

  given (self: Refined) {
    def tpt(given ctx: Context): TypeTree = internal.Refined_tpt(self)
    def refinements(given ctx: Context): List[Definition] = internal.Refined_refinements(self)
  }

  object IsApplied {
    /** Matches any Applied and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[Applied] =
      internal.matchApplied(tree)
  }

  object Applied {
    def apply(tpt: TypeTree, args: List[Tree /*TypeTree | TypeBoundsTree*/])(given ctx: Context): Applied =
      internal.Applied_apply(tpt, args)
    def copy(original: Tree)(tpt: TypeTree, args: List[Tree /*TypeTree | TypeBoundsTree*/])(given ctx: Context): Applied =
      internal.Applied_copy(original)(tpt, args)
    def unapply(tree: Tree)(given ctx: Context): Option[(TypeTree, List[Tree /*TypeTree | TypeBoundsTree*/])] =
      internal.matchApplied(tree).map(x => (x.tpt, x.args))
  }

  given (self: Applied) {
    def tpt(given ctx: Context): TypeTree = internal.Applied_tpt(self)
    def args(given ctx: Context): List[Tree /*TypeTree | TypeBoundsTree*/] = internal.Applied_args(self)
  }

  object IsAnnotated {
    /** Matches any Annotated and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[Annotated] =
      internal.matchAnnotated(tree)
  }

  object Annotated {
    def apply(arg: TypeTree, annotation: Term)(given ctx: Context): Annotated =
      internal.Annotated_apply(arg, annotation)
    def copy(original: Tree)(arg: TypeTree, annotation: Term)(given ctx: Context): Annotated =
      internal.Annotated_copy(original)(arg, annotation)
    def unapply(tree: Tree)(given ctx: Context): Option[(TypeTree, Term)] =
      internal.matchAnnotated(tree).map(x => (x.arg, x.annotation))
  }

  given (self: Annotated) {
    def arg(given ctx: Context): TypeTree = internal.Annotated_arg(self)
    def annotation(given ctx: Context): Term = internal.Annotated_annotation(self)
  }

  object IsMatchTypeTree {
    /** Matches any MatchTypeTree and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[MatchTypeTree] =
      internal.matchMatchTypeTree(tree)
  }

  object MatchTypeTree {
    def apply(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef])(given ctx: Context): MatchTypeTree =
      internal.MatchTypeTree_apply(bound, selector, cases)
    def copy(original: Tree)(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef])(given ctx: Context): MatchTypeTree =
      internal.MatchTypeTree_copy(original)(bound, selector, cases)
    def unapply(tree: Tree)(given ctx: Context): Option[(Option[TypeTree], TypeTree, List[TypeCaseDef])] =
      internal.matchMatchTypeTree(tree).map(x => (x.bound, x.selector, x.cases))
  }

  given (self: MatchTypeTree) {
    def bound(given ctx: Context): Option[TypeTree] = internal.MatchTypeTree_bound(self)
    def selector(given ctx: Context): TypeTree = internal.MatchTypeTree_selector(self)
    def cases(given ctx: Context): List[TypeCaseDef] = internal.MatchTypeTree_cases(self)
  }

  object IsByName {
    /** Matches any ByName and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[ByName] =
      internal.matchByName(tree)
  }

  object ByName {
    def apply(result: TypeTree)(given ctx: Context): ByName =
      internal.ByName_apply(result)
    def copy(original: Tree)(result: TypeTree)(given ctx: Context): ByName =
      internal.ByName_copy(original)(result)
    def unapply(tree: Tree)(given ctx: Context): Option[TypeTree] =
      internal.matchByName(tree).map(_.result)
  }

  given (self: ByName) {
    def result(given ctx: Context): TypeTree = internal.ByName_result(self)
  }

  object IsLambdaTypeTree {
    /** Matches any LambdaTypeTree and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[LambdaTypeTree] =
      internal.matchLambdaTypeTree(tree)
  }

  object LambdaTypeTree {
    def apply(tparams: List[TypeDef], body: Tree /*TypeTree | TypeBoundsTree*/)(given ctx: Context): LambdaTypeTree =
      internal.Lambdaapply(tparams, body)
    def copy(original: Tree)(tparams: List[TypeDef], body: Tree /*TypeTree | TypeBoundsTree*/)(given ctx: Context): LambdaTypeTree =
      internal.Lambdacopy(original)(tparams, body)
    def unapply(tree: Tree)(given ctx: Context): Option[(List[TypeDef], Tree /*TypeTree | TypeBoundsTree*/)] =
      internal.matchLambdaTypeTree(tree).map(x => (x.tparams, x.body))
  }

  given (self: LambdaTypeTree) {
    def tparams(given ctx: Context): List[TypeDef] = internal.Lambdatparams(self)
    def body(given ctx: Context): Tree /*TypeTree | TypeBoundsTree*/ = internal.Lambdabody(self)
  }

  object IsTypeBind {
    /** Matches any TypeBind and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[TypeBind] =
      internal.matchTypeBind(tree)
  }

  object TypeBind {
    // TODO def apply(name: String, tree: Tree)(given ctx: Context): TypeBind
    def copy(original: Tree)(name: String, tpt: Tree /*TypeTree | TypeBoundsTree*/)(given ctx: Context): TypeBind =
      internal.TypeBind_copy(original)(name, tpt)
    def unapply(tree: Tree)(given ctx: Context): Option[(String, Tree /*TypeTree | TypeBoundsTree*/)] =
      internal.matchTypeBind(tree).map(x => (x.name, x.body))
  }

  given (self: TypeBind) {
    def name(given ctx: Context): String = internal.TypeBind_name(self)
    def body(given ctx: Context): Tree /*TypeTree | TypeBoundsTree*/ = internal.TypeBind_body(self)
  }

  object IsTypeBlock {
    /** Matches any TypeBlock and returns it */
    def unapply(tree: Tree)(given ctx: Context): Option[TypeBlock] =
      internal.matchTypeBlock(tree)
  }

  object TypeBlock {
    def apply(aliases: List[TypeDef], tpt: TypeTree)(given ctx: Context): TypeBlock =
      internal.TypeBlock_apply(aliases, tpt)
    def copy(original: Tree)(aliases: List[TypeDef], tpt: TypeTree)(given ctx: Context): TypeBlock =
      internal.TypeBlock_copy(original)(aliases, tpt)
    def unapply(tree: Tree)(given ctx: Context): Option[(List[TypeDef], TypeTree)] =
      internal.matchTypeBlock(tree).map(x => (x.aliases, x.tpt))
  }

  given (self: TypeBlock) {
    def aliases(given ctx: Context): List[TypeDef] = internal.TypeBlock_aliases(self)
    def tpt(given ctx: Context): TypeTree = internal.TypeBlock_tpt(self)
  }

  // ----- TypeBoundsTrees ------------------------------------------------

  given (self: TypeBoundsTree) {
    def tpe(given ctx: Context): TypeBounds = internal.TypeBoundsTree_tpe(self)
    def low(given ctx: Context): TypeTree = internal.TypeBoundsTree_low(self)
    def hi(given ctx: Context): TypeTree = internal.TypeBoundsTree_hi(self)
  }

  object IsTypeBoundsTree {
    def unapply(tree: Tree)(given ctx: Context): Option[TypeBoundsTree] =
      internal.matchTypeBoundsTree(tree)
  }

  object TypeBoundsTree {
    def unapply(tree: Tree)(given ctx: Context): Option[(TypeTree, TypeTree)] =
      internal.matchTypeBoundsTree(tree).map(x => (x.low, x.hi))
  }

  given (self: WildcardTypeTree) {
    def tpe(given ctx: Context): TypeOrBounds = internal.WildcardTypeTree_tpe(self)
  }

  object IsWildcardTypeTree {
    def unapply(tree: Tree)(given ctx: Context): Option[WildcardTypeTree] =
      internal.matchWildcardTypeTree(tree)
  }

  /** TypeBoundsTree containing wildcard type bounds */
  object WildcardTypeTree {
    /** Matches a TypeBoundsTree containing wildcard type bounds */
    def unapply(tree: Tree)(given ctx: Context): Boolean =
      internal.matchWildcardTypeTree(tree).isDefined
  }

  // ----- CaseDefs ------------------------------------------------

  given (caseDef: CaseDef) {
    def pattern(given ctx: Context): Tree = internal.CaseDef_pattern(caseDef)
    def guard(given ctx: Context): Option[Term] = internal.CaseDef_guard(caseDef)
    def rhs(given ctx: Context): Term = internal.CaseDef_rhs(caseDef)
  }

  object IsCaseDef {
    def unapply(self: Tree)(given ctx: Context): Option[CaseDef] =
      internal.matchCaseDef(self)
  }

  object CaseDef {
    def apply(pattern: Tree, guard: Option[Term], rhs: Term)(given ctx: Context): CaseDef =
      internal.CaseDef_module_apply(pattern, guard, rhs)

    def copy(original: Tree)(pattern: Tree, guard: Option[Term], rhs: Term)(given ctx: Context): CaseDef =
      internal.CaseDef_module_copy(original)(pattern, guard, rhs)

    def unapply(tree: Tree)(given ctx: Context): Option[(Tree, Option[Term], Term)] =
      internal.matchCaseDef(tree).map( x => (x.pattern, x.guard, x.rhs))
  }

  given (caseDef: TypeCaseDef) {
    def pattern(given ctx: Context): TypeTree = internal.TypeCaseDef_pattern(caseDef)
    def rhs(given ctx: Context): TypeTree = internal.TypeCaseDef_rhs(caseDef)
  }

  object IsTypeCaseDef {
    def unapply(self: Tree)(given ctx: Context): Option[TypeCaseDef] =
      internal.matchTypeCaseDef(self)
  }

  object TypeCaseDef {
    def apply(pattern: TypeTree, rhs: TypeTree)(given ctx: Context): TypeCaseDef =
      internal.TypeCaseDef_module_apply(pattern, rhs)

    def copy(original: Tree)(pattern: TypeTree, rhs: TypeTree)(given ctx: Context): TypeCaseDef =
      internal.TypeCaseDef_module_copy(original)(pattern, rhs)

    def unapply(tree: Tree)(given ctx: Context): Option[(TypeTree, TypeTree)] =
      internal.matchTypeCaseDef(tree).map( x => (x.pattern, x.rhs))
  }

  // ----- Trees ------------------------------------------------

  object IsBind {
    def unapply(pattern: Tree)(given ctx: Context): Option[Bind] =
      internal.matchTree_Bind(pattern)
  }

  object Bind {
    // TODO def apply(name: String, pattern: Tree)(given ctx: Context): Bind
    def copy(original: Tree)(name: String, pattern: Tree)(given ctx: Context): Bind =
      internal.Tree_Bind_module_copy(original)(name, pattern)
    def unapply(pattern: Tree)(given ctx: Context): Option[(String, Tree)] =
      internal.matchTree_Bind(pattern).map(x => (x.name, x.pattern))
  }

  given (bind: Bind) {
    def name(given ctx: Context): String = internal.Tree_Bind_name(bind)
    def pattern(given ctx: Context): Tree = internal.Tree_Bind_pattern(bind)
  }

  object IsUnapply {
    def unapply(pattern: Tree)(given ctx: Context): Option[Unapply] =
      internal.matchTree_Unapply(pattern)
  }

  object Unapply {
    // TODO def apply(fun: Term, implicits: List[Term], patterns: List[Tree])(given ctx: Context): Unapply
    def copy(original: Tree)(fun: Term, implicits: List[Term], patterns: List[Tree])(given ctx: Context): Unapply =
      internal.Tree_Unapply_module_copy(original)(fun, implicits, patterns)
    def unapply(pattern: Tree)(given ctx: Context): Option[(Term, List[Term], List[Tree])] =
      internal.matchTree_Unapply(pattern).map(x => (x.fun, x.implicits, x.patterns))
  }

  given (unapply: Unapply) {
    def fun(given ctx: Context): Term = internal.Tree_Unapply_fun(unapply)
    def implicits(given ctx: Context): List[Term] = internal.Tree_Unapply_implicits(unapply)
    def patterns(given ctx: Context): List[Tree] = internal.Tree_Unapply_patterns(unapply)
  }

  object IsAlternatives {
    def unapply(pattern: Tree)(given ctx: Context): Option[Alternatives] =
      internal.matchTree_Alternatives(pattern)
  }

  object Alternatives {
    def apply(patterns: List[Tree])(given ctx: Context): Alternatives =
      internal.Tree_Alternatives_module_apply(patterns)
    def copy(original: Tree)(patterns: List[Tree])(given ctx: Context): Alternatives =
      internal.Tree_Alternatives_module_copy(original)(patterns)
    def unapply(pattern: Tree)(given ctx: Context): Option[List[Tree]] =
      internal.matchTree_Alternatives(pattern).map(_.patterns)
  }

  given (alternatives: Alternatives) {
    def patterns(given ctx: Context): List[Tree] = internal.Tree_Alternatives_patterns(alternatives)
  }


}
