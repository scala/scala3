package scala.tasty
package reflect

trait TreeOps extends Core {

  // ----- Tree -----------------------------------------------------

  given TreeOps: extension (self: Tree) {
    /** Position in the source code */
    def pos(given ctx: Context): Position = internal.Tree_pos(self)

    def symbol(given ctx: Context): Symbol = internal.Tree_symbol(self)
  }

  given (given Context): IsInstanceOf[PackageClause] = internal.isInstanceOfPackageClause

  object IsPackageClause
    @deprecated("Use _: PackageClause", "")
    def unapply(x: PackageClause): Some[PackageClause] = Some(x)

  object PackageClause {
    def apply(pid: Ref, stats: List[Tree])(given ctx: Context): PackageClause =
      internal.PackageClause_apply(pid, stats)
    def copy(original: Tree)(pid: Ref, stats: List[Tree])(given ctx: Context): PackageClause =
      internal.PackageClause_copy(original)(pid, stats)
    def unapply(tree: PackageClause)(given ctx: Context): Some[(Ref, List[Tree])] =
      Some((tree.pid, tree.stats))
  }

  given PackageClauseOps: extension (self: PackageClause) {
    def pid(given ctx: Context): Ref = internal.PackageClause_pid(self)
    def stats(given ctx: Context): List[Tree] = internal.PackageClause_stats(self)
  }

  given (given Context): IsInstanceOf[Import] = internal.isInstanceOfImport

  object IsImport
    @deprecated("Use _: Import", "")
    def unapply(x: Import): Some[Import] = Some(x)

  object Import {
    def apply(expr: Term, selectors: List[ImportSelector])(given ctx: Context): Import =
      internal.Import_apply(expr, selectors)
    def copy(original: Tree)(expr: Term, selectors: List[ImportSelector])(given ctx: Context): Import =
      internal.Import_copy(original)(expr, selectors)
    def unapply(tree: Import)(given ctx: Context): Option[(Term, List[ImportSelector])] =
      Some((tree.expr, tree.selectors))
  }

  given ImportOps: extension (self: Import)  {
    def expr(given ctx: Context): Term = internal.Import_expr(self)
    def selectors(given ctx: Context): List[ImportSelector] =
      internal.Import_selectors(self)
  }

  given (given Context): IsInstanceOf[Statement] = internal.isInstanceOfStatement

  object IsStatement
    @deprecated("Use _: Statement", "")
    def unapply(x: Statement): Option[Statement] = Some(x)

  // ----- Definitions ----------------------------------------------

  given (given Context): IsInstanceOf[Definition] = internal.isInstanceOfDefinition

  object IsDefinition
    @deprecated("Use _: Definition", "")
    def unapply(x: Definition): Option[Definition] = Some(x)

  given DefinitionOps: extension (self: Definition) {
    def name(given ctx: Context): String = internal.Definition_name(self)
  }

  // ClassDef

  given (given Context): IsInstanceOf[ClassDef] = internal.isInstanceOfClassDef

  object IsClassDef
    @deprecated("Use _: ClassDef", "")
    def unapply(x: ClassDef): Some[ClassDef] = Some(x)

  object ClassDef {
    // TODO def apply(name: String, constr: DefDef, parents: List[TermOrTypeTree], selfOpt: Option[ValDef], body: List[Statement])(given ctx: Context): ClassDef
    def copy(original: Tree)(name: String, constr: DefDef, parents: List[Tree /* Term | TypeTree */], derived: List[TypeTree], selfOpt: Option[ValDef], body: List[Statement])(given ctx: Context): ClassDef =
      internal.ClassDef_copy(original)(name, constr, parents, derived, selfOpt, body)
    def unapply(cdef: ClassDef)(given ctx: Context): Option[(String, DefDef, List[Tree /* Term | TypeTree */], List[TypeTree], Option[ValDef], List[Statement])] =
      Some((cdef.name, cdef.constructor, cdef.parents, cdef.derived, cdef.self, cdef.body))
  }

  given ClassDefOps: extension (self: ClassDef) {
    def constructor(given ctx: Context): DefDef = internal.ClassDef_constructor(self)
    def parents(given ctx: Context): List[Tree /* Term | TypeTree */] = internal.ClassDef_parents(self)
    def derived(given ctx: Context): List[TypeTree] = internal.ClassDef_derived(self)
    def self(given ctx: Context): Option[ValDef] = internal.ClassDef_self(self)
    def body(given ctx: Context): List[Statement] = internal.ClassDef_body(self)
  }

  // DefDef

  given (given Context): IsInstanceOf[DefDef] = internal.isInstanceOfDefDef

  object IsDefDef
    @deprecated("Use _: DefDef", "")
    def unapply(x: DefDef): Some[DefDef] = Some(x)

  object DefDef {
    def apply(symbol: Symbol, rhsFn: List[Type] => List[List[Term]] => Option[Term])(given ctx: Context): DefDef =
      internal.DefDef_apply(symbol, rhsFn)
    def copy(original: Tree)(name: String, typeParams: List[TypeDef], paramss: List[List[ValDef]], tpt: TypeTree, rhs: Option[Term])(given ctx: Context): DefDef =
      internal.DefDef_copy(original)(name, typeParams, paramss, tpt, rhs)
    def unapply(ddef: DefDef)(given ctx: Context): Option[(String, List[TypeDef], List[List[ValDef]], TypeTree, Option[Term])] =
      Some((ddef.name, ddef.typeParams, ddef.paramss, ddef.returnTpt, ddef.rhs))
  }

  given DefDefOps: extension (self: DefDef) {
    def typeParams(given ctx: Context): List[TypeDef] = internal.DefDef_typeParams(self)
    def paramss(given ctx: Context): List[List[ValDef]] = internal.DefDef_paramss(self)
    def returnTpt(given ctx: Context): TypeTree = internal.DefDef_returnTpt(self) // TODO rename to tpt
    def rhs(given ctx: Context): Option[Term] = internal.DefDef_rhs(self)
  }

  // ValDef

  given (given Context): IsInstanceOf[ValDef] = internal.isInstanceOfValDef

  object IsValDef
    @deprecated("Use _: ValDef", "")
    def unapply(x: ValDef): Some[ValDef] = Some(x)

  object ValDef {
    def apply(symbol: Symbol, rhs: Option[Term])(given ctx: Context): ValDef =
      internal.ValDef_apply(symbol, rhs)
    def copy(original: Tree)(name: String, tpt: TypeTree, rhs: Option[Term])(given ctx: Context): ValDef =
      internal.ValDef_copy(original)(name, tpt, rhs)
    def unapply(vdef: ValDef)(given ctx: Context): Option[(String, TypeTree, Option[Term])] =
      Some((vdef.name, vdef.tpt, vdef.rhs))
  }

  given ValDefOps: extension (self: ValDef) {
    def tpt(given ctx: Context): TypeTree = internal.ValDef_tpt(self)
    def rhs(given ctx: Context): Option[Term] = internal.ValDef_rhs(self)
  }

  // TypeDef

  given (given Context): IsInstanceOf[TypeDef] = internal.isInstanceOfTypeDef

  object IsTypeDef
    @deprecated("Use _: TypeDef", "")
    def unapply(x: TypeDef): Some[TypeDef] = Some(x)

  object TypeDef {
    def apply(symbol: Symbol)(given ctx: Context): TypeDef =
      internal.TypeDef_apply(symbol)
    def copy(original: Tree)(name: String, rhs: Tree /*TypeTree | TypeBoundsTree*/)(given ctx: Context): TypeDef =
      internal.TypeDef_copy(original)(name, rhs)
    def unapply(tdef: TypeDef)(given ctx: Context): Option[(String, Tree /*TypeTree | TypeBoundsTree*/ /* TypeTree | TypeBoundsTree */)] =
      Some((tdef.name, tdef.rhs))
  }

  given TypeDefOps: extension (self: TypeDef) {
    def rhs(given ctx: Context): Tree /*TypeTree | TypeBoundsTree*/ = internal.TypeDef_rhs(self)
  }

  // PackageDef

  given (given Context): IsInstanceOf[PackageDef] = internal.isInstanceOfPackageDef

  given PackageDefOps: extension (self: PackageDef) {
    def owner(given ctx: Context): PackageDef = internal.PackageDef_owner(self)
    def members(given ctx: Context): List[Statement] = internal.PackageDef_members(self)
  }

  object IsPackageDef
    @deprecated("Use _: PackageDef", "")
    def unapply(x: PackageDef): Some[PackageDef] = Some(x)

  object PackageDef {
    def unapply(tree: PackageDef)(given ctx: Context): Option[(String, PackageDef)] =
      Some((tree.name, tree.owner))
  }

  // ----- Terms ----------------------------------------------------

  given TermOps: extension (self: Term) {
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

  given (given Context): IsInstanceOf[Term] = internal.isInstanceOfTerm

  object IsTerm
    @deprecated("Use _: Term", "")
    def unapply(x: Term): Some[Term] = Some(x)

  given (given Context): IsInstanceOf[Ref] = internal.isInstanceOfRef

  object IsRef
    @deprecated("Use _: Ref", "")
    def unapply(x: Ref): Some[Ref] = Some(x)

  object Ref {

    /** Create a reference tree */
    def apply(sym: Symbol)(given ctx: Context): Ref =
      internal.Ref_apply(sym)

    // TODO def copy(original: Tree)(name: String)(given ctx: Context): Ref

  }

  given (given Context): IsInstanceOf[Ident] = internal.isInstanceOfIdent

  given IdentOps: extension (self: Ident) {
    def name(given ctx: Context): String = internal.Ident_name(self)
  }

  /** Scala term identifier */
  object IsIdent
    @deprecated("Use _: Ident", "")
    def unapply(x: Ident): Some[Ident] = Some(x)

  object Ident {
    def apply(tmref: TermRef)(given ctx: Context): Term =
      internal.Ident_apply(tmref)

    def copy(original: Tree)(name: String)(given ctx: Context): Ident =
      internal.Ident_copy(original)(name)

    /** Matches a term identifier and returns its name */
    def unapply(tree: Ident)(given ctx: Context): Option[String] =
      Some(tree.name)
  }

  given (given Context): IsInstanceOf[Select] = internal.isInstanceOfSelect

  /** Scala term selection */
  object IsSelect
    @deprecated("Use _: Select", "")
    def unapply(x: Select): Some[Select] = Some(x)

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
    def unapply(x: Select)(given ctx: Context): Option[(Term, String)] =
      Some((x.qualifier, x.name))
  }

  given SelectOps: extension (self: Select) {
    def qualifier(given ctx: Context): Term = internal.Select_qualifier(self)
    def name(given ctx: Context): String = internal.Select_name(self)
    def signature(given ctx: Context): Option[Signature] = internal.Select_signature(self)
  }

  given (given Context): IsInstanceOf[Literal] =
    internal.isInstanceOfLiteral

  /** Scala literal constant */
  object IsLiteral
    @deprecated("Use _: Literal", "")
    def unapply(x: Literal): Some[Literal] = Some(x)

  object Literal {

    /** Create a literal constant */
    def apply(constant: Constant)(given ctx: Context): Literal =
      internal.Literal_apply(constant)

    def copy(original: Tree)(constant: Constant)(given ctx: Context): Literal =
      internal.Literal_copy(original)(constant)

    /** Matches a literal constant */
    def unapply(x: Literal)(given ctx: Context): Option[Constant] =
      Some(x.constant)
  }

  given LiteralOps: extension (self: Literal) {
    def constant(given ctx: Context): Constant = internal.Literal_constant(self)
  }

  given (given Context): IsInstanceOf[This] = internal.isInstanceOfThis

  /** Scala `this` or `this[id]` */
  object IsThis
    @deprecated("Use _: This", "")
    def unapply(x: This): Some[This] = Some(x)

  object This {

    /** Create a `this[<id: Id]>` */
    def apply(cls: Symbol)(given ctx: Context): This =
      internal.This_apply(cls)

    def copy(original: Tree)(qual: Option[Id])(given ctx: Context): This =
      internal.This_copy(original)(qual)

    /** Matches `this[<id: Option[Id]>` */
    def unapply(x: This)(given ctx: Context): Option[Option[Id]] = Some(x.id)

  }

  given ThisOps: extension (self: This) {
    def id(given ctx: Context): Option[Id] = internal.This_id(self)
  }

  given (given Context): IsInstanceOf[New] = internal.isInstanceOfNew

  /** Scala `new` */
  object IsNew
    @deprecated("Use _: New", "")
    def unapply(x: New): Some[New] = Some(x)

  object New {

    /** Create a `new <tpt: TypeTree>` */
    def apply(tpt: TypeTree)(given ctx: Context): New =
      internal.New_apply(tpt)

    def copy(original: Tree)(tpt: TypeTree)(given ctx: Context): New =
      internal.New_copy(original)(tpt)

    /** Matches a `new <tpt: TypeTree>` */
    def unapply(x: New)(given ctx: Context): Option[TypeTree] = Some(x.tpt)
  }

  given NewOps: extension (self: New) {
    def tpt(given ctx: Context): TypeTree = internal.New_tpt(self)
  }

  given (given Context): IsInstanceOf[NamedArg] = internal.isInstanceOfNamedArg

  /** Scala named argument `x = y` in argument position */
  object IsNamedArg
    @deprecated("Use _: NamedArg", "")
    def unapply(x: NamedArg): Some[NamedArg] = Some(x)

  object NamedArg {

    /** Create a named argument `<name: String> = <value: Term>` */
    def apply(name: String, arg: Term)(given ctx: Context): NamedArg =
      internal.NamedArg_apply(name, arg)

    def copy(original: Tree)(name: String, arg: Term)(given ctx: Context): NamedArg =
      internal.NamedArg_copy(original)(name, arg)

    /** Matches a named argument `<name: String> = <value: Term>` */
    def unapply(x: NamedArg)(given ctx: Context): Option[(String, Term)] =
      Some((x.name, x.value))

  }

  given NamedArgOps: extension (self: NamedArg) {
    def name(given ctx: Context): String = internal.NamedArg_name(self)
    def value(given ctx: Context): Term = internal.NamedArg_value(self)
  }

  given (given Context): IsInstanceOf[Apply] = internal.isInstanceOfApply

  /** Scala parameter application */
  object IsApply
    @deprecated("Use _: Apply", "")
    def unapply(x: Apply): Some[Apply] = Some(x)

  object Apply {

    /** Create a function application `<fun: Term>(<args: List[Term]>)` */
    def apply(fun: Term, args: List[Term])(given ctx: Context): Apply =
      internal.Apply_apply(fun, args)

    def copy(original: Tree)(fun: Term, args: List[Term])(given ctx: Context): Apply =
      internal.Apply_copy(original)(fun, args)

    /** Matches a function application `<fun: Term>(<args: List[Term]>)` */
    def unapply(x: Apply)(given ctx: Context): Option[(Term, List[Term])] =
      Some((x.fun, x.args))
  }

  given ApplyOps: extension (self: Apply) {
    def fun(given ctx: Context): Term = internal.Apply_fun(self)
    def args(given ctx: Context): List[Term] = internal.Apply_args(self)
  }

  given (given Context): IsInstanceOf[TypeApply] = internal.isInstanceOfTypeApply

  /** Scala type parameter application */
  object IsTypeApply
    @deprecated("Use _: TypeApply", "")
    def unapply(x: TypeApply): Some[TypeApply] = Some(x)

  object TypeApply {

    /** Create a function type application `<fun: Term>[<args: List[TypeTree]>]` */
    def apply(fun: Term, args: List[TypeTree])(given ctx: Context): TypeApply =
      internal.TypeApply_apply(fun, args)

    def copy(original: Tree)(fun: Term, args: List[TypeTree])(given ctx: Context): TypeApply =
      internal.TypeApply_copy(original)(fun, args)

    /** Matches a function type application `<fun: Term>[<args: List[TypeTree]>]` */
    def unapply(x: TypeApply)(given ctx: Context): Option[(Term, List[TypeTree])] =
      Some((x.fun, x.args))

  }

  given TypeApplyOps: extension (self: TypeApply) {
    def fun(given ctx: Context): Term = internal.TypeApply_fun(self)
    def args(given ctx: Context): List[TypeTree] = internal.TypeApply_args(self)
  }

  given (given Context): IsInstanceOf[Super] = internal.isInstanceOfSuper

  /** Scala `x.super` or `x.super[id]` */
  object IsSuper
    @deprecated("Use _: Super", "")
    def unapply(x: Super): Some[Super] = Some(x)

  object Super {

    /** Creates a `<qualifier: Term>.super[<id: Option[Id]>` */
    def apply(qual: Term, mix: Option[Id])(given ctx: Context): Super =
      internal.Super_apply(qual, mix)

    def copy(original: Tree)(qual: Term, mix: Option[Id])(given ctx: Context): Super =
      internal.Super_copy(original)(qual, mix)

    /** Matches a `<qualifier: Term>.super[<id: Option[Id]>` */
    def unapply(x: Super)(given ctx: Context): Option[(Term, Option[Id])] =
      Some((x.qualifier, x.id))
  }

  given SuperOps: extension (self: Super) {
    def qualifier(given ctx: Context): Term = internal.Super_qualifier(self)
    def id(given ctx: Context): Option[Id] = internal.Super_id(self)
  }

  given (given Context): IsInstanceOf[Typed] = internal.isInstanceOfTyped

  /** Scala ascription `x: T` */
  object IsTyped
    @deprecated("Use _: Typed", "")
    def unapply(x: Typed): Some[Typed] = Some(x)

  object Typed {

    /** Create a type ascription `<x: Term>: <tpt: TypeTree>` */
    def apply(expr: Term, tpt: TypeTree)(given ctx: Context): Typed =
      internal.Typed_apply(expr, tpt)

    def copy(original: Tree)(expr: Term, tpt: TypeTree)(given ctx: Context): Typed =
      internal.Typed_copy(original)(expr, tpt)

    /** Matches `<expr: Term>: <tpt: TypeTree>` */
    def unapply(x: Typed)(given ctx: Context): Option[(Term, TypeTree)] =
      Some((x.expr, x.tpt))

  }

  given TypedOps: extension (self: Typed) {
    def expr(given ctx: Context): Term = internal.Typed_expr(self)
    def tpt(given ctx: Context): TypeTree = internal.Typed_tpt(self)
  }

  given (given Context): IsInstanceOf[Assign] = internal.isInstanceOfAssign

  /** Scala assign `x = y` */
  object IsAssign
    @deprecated("Use _: Assign", "")
    def unapply(x: Assign): Some[Assign] = Some(x)

  object Assign {

    /** Create an assignment `<lhs: Term> = <rhs: Term>` */
    def apply(lhs: Term, rhs: Term)(given ctx: Context): Assign =
      internal.Assign_apply(lhs, rhs)

    def copy(original: Tree)(lhs: Term, rhs: Term)(given ctx: Context): Assign =
      internal.Assign_copy(original)(lhs, rhs)

    /** Matches an assignment `<lhs: Term> = <rhs: Term>` */
    def unapply(x: Assign)(given ctx: Context): Option[(Term, Term)] =
      Some((x.lhs, x.rhs))
  }

  given AssignOps: extension (self: Assign) {
    def lhs(given ctx: Context): Term = internal.Assign_lhs(self)
    def rhs(given ctx: Context): Term = internal.Assign_rhs(self)
  }

  given (given Context): IsInstanceOf[Block] = internal.isInstanceOfBlock

  /** Scala code block `{ stat0; ...; statN; expr }` term */
  object IsBlock
    @deprecated("Use _: Block", "")
    def unapply(x: Block): Some[Block] = Some(x)

  object Block {

    /** Creates a block `{ <statements: List[Statement]>; <expr: Term> }` */
    def apply(stats: List[Statement], expr: Term)(given ctx: Context): Block =
      internal.Block_apply(stats, expr)

    def copy(original: Tree)(stats: List[Statement], expr: Term)(given ctx: Context): Block =
      internal.Block_copy(original)(stats, expr)

    /** Matches a block `{ <statements: List[Statement]>; <expr: Term> }` */
    def unapply(x: Block)(given ctx: Context): Option[(List[Statement], Term)] =
      Some((x.statements, x.expr))
  }

  given BlockOps: extension (self: Block) {
    def statements(given ctx: Context): List[Statement] = internal.Block_statements(self)
    def expr(given ctx: Context): Term = internal.Block_expr(self)
  }

  given (given Context): IsInstanceOf[Closure] = internal.isInstanceOfClosure

  object IsClosure
    @deprecated("Use _: Closure", "")
    def unapply(x: Closure): Some[Closure] = Some(x)

  object Closure {

    def apply(meth: Term, tpt: Option[Type])(given ctx: Context): Closure =
      internal.Closure_apply(meth, tpt)

    def copy(original: Tree)(meth: Tree, tpt: Option[Type])(given ctx: Context): Closure =
      internal.Closure_copy(original)(meth, tpt)

    def unapply(x: Closure)(given ctx: Context): Option[(Term, Option[Type])] =
      Some((x.meth, x.tpeOpt))
  }

  given ClosureOps: extension (self: Closure) {
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

    def apply(tpe: MethodType, rhsFn: List[Tree] => Tree)(implicit ctx: Context): Block =
      internal.Lambda_apply(tpe, rhsFn)

  }

  given (given Context): IsInstanceOf[If] = internal.isInstanceOfIf

  object IsIf
    @deprecated("Use _: If", "")
    def unapply(x: If): Some[If] = Some(x)

  /** Scala `if`/`else` term */
  object If {

    /** Create an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>` */
    def apply(cond: Term, thenp: Term, elsep: Term)(given ctx: Context): If =
      internal.If_apply(cond, thenp, elsep)

    def copy(original: Tree)(cond: Term, thenp: Term, elsep: Term)(given ctx: Context): If =
      internal.If_copy(original)(cond, thenp, elsep)

    /** Matches an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>` */
    def unapply(tree: If)(given ctx: Context): Option[(Term, Term, Term)] =
      Some((tree.cond, tree.thenp, tree.elsep))

  }

  given IfOps: extension (self: If) {
    def cond(given ctx: Context): Term = internal.If_cond(self)
    def thenp(given ctx: Context): Term = internal.If_thenp(self)
    def elsep(given ctx: Context): Term = internal.If_elsep(self)
  }

  given (given Context): IsInstanceOf[Match] = internal.isInstanceOfMatch

  /** Scala `match` term */
  object IsMatch
    @deprecated("Use _: Match", "")
    def unapply(x: Match): Some[Match] = Some(x)

  object Match {

    /** Creates a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }` */
    def apply(selector: Term, cases: List[CaseDef])(given ctx: Context): Match =
      internal.Match_apply(selector, cases)

    def copy(original: Tree)(selector: Term, cases: List[CaseDef])(given ctx: Context): Match =
      internal.Match_copy(original)(selector, cases)

    /** Matches a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }` */
    def unapply(x: Match)(given ctx: Context): Option[(Term, List[CaseDef])] =
      Some((x.scrutinee, x.cases))

  }

  given MatchOps: extension (self: Match) {
    def scrutinee(given ctx: Context): Term = internal.Match_scrutinee(self)
    def cases(given ctx: Context): List[CaseDef] = internal.Match_cases(self)
  }

  given (given Context): IsInstanceOf[GivenMatch] = internal.isInstanceOfGivenMatch

  /** Scala implicit `match` term */
  object IsGivenMatch
    @deprecated("Use _: GivenMatch", "")
    def unapply(x: GivenMatch): Some[GivenMatch] = Some(x)

  object GivenMatch {

    /** Creates a pattern match `given match { <cases: List[CaseDef]> }` */
    def apply(cases: List[CaseDef])(given ctx: Context): GivenMatch =
      internal.GivenMatch_apply(cases)

    def copy(original: Tree)(cases: List[CaseDef])(given ctx: Context): GivenMatch =
      internal.GivenMatch_copy(original)(cases)

    /** Matches a pattern match `given match { <cases: List[CaseDef]> }` */
    def unapply(x: GivenMatch)(given ctx: Context): Option[List[CaseDef]] = Some(x.cases)

  }

  given GivenMatchOps: extension (self: GivenMatch) {
    def cases(given ctx: Context): List[CaseDef] = internal.GivenMatch_cases(self)
  }

  given (given Context): IsInstanceOf[Try] = internal.isInstanceOfTry

  /** Scala `try`/`catch`/`finally` term */
  object IsTry
    @deprecated("Use _: Try", "")
    def unapply(x: Try): Some[Try] = Some(x)

  object Try {

    /** Create a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>` */
    def apply(expr: Term, cases: List[CaseDef], finalizer: Option[Term])(given ctx: Context): Try =
      internal.Try_apply(expr, cases, finalizer)

    def copy(original: Tree)(expr: Term, cases: List[CaseDef], finalizer: Option[Term])(given ctx: Context): Try =
      internal.Try_copy(original)(expr, cases, finalizer)

    /** Matches a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>` */
    def unapply(x: Try)(given ctx: Context): Option[(Term, List[CaseDef], Option[Term])] =
      Some((x.body, x.cases, x.finalizer))

  }

  given TryOps: extension (self: Try) {
    def body(given ctx: Context): Term = internal.Try_body(self)
    def cases(given ctx: Context): List[CaseDef] = internal.Try_cases(self)
    def finalizer(given ctx: Context): Option[Term] = internal.Try_finalizer(self)
  }

  given (given Context): IsInstanceOf[Return] = internal.isInstanceOfReturn

  /** Scala local `return` */
  object IsReturn
    @deprecated("Use _: Return", "")
    def unapply(x: Return): Some[Return] = Some(x)

  object Return {

    /** Creates `return <expr: Term>` */
    def apply(expr: Term)(given ctx: Context): Return =
      internal.Return_apply(expr)

    def copy(original: Tree)(expr: Term)(given ctx: Context): Return =
      internal.Return_copy(original)(expr)

    /** Matches `return <expr: Term>` */
    def unapply(x: Return)(given ctx: Context): Option[Term] = Some(x.expr)

  }

  given ReturnOps: extension (self: Return) {
    def expr(given ctx: Context): Term = internal.Return_expr(self)
  }

  given (given Context): IsInstanceOf[Repeated] = internal.isInstanceOfRepeated

  object IsRepeated
    @deprecated("Use _: Repeated", "")
    def unapply(x: Repeated): Some[Repeated] = Some(x)

  object Repeated {

    def apply(elems: List[Term], tpt: TypeTree)(given ctx: Context): Repeated =
      internal.Repeated_apply(elems, tpt)

    def copy(original: Tree)(elems: List[Term], tpt: TypeTree)(given ctx: Context): Repeated =
      internal.Repeated_copy(original)(elems, tpt)

    def unapply(x: Repeated)(given ctx: Context): Option[(List[Term], TypeTree)] =
      Some((x.elems, x.elemtpt))

  }

  given RepeatedOps: extension (self: Repeated) {
    def elems(given ctx: Context): List[Term] = internal.Repeated_elems(self)
    def elemtpt(given ctx: Context): TypeTree = internal.Repeated_elemtpt(self)
  }

  given (given Context): IsInstanceOf[Inlined] = internal.isInstanceOfInlined

  object IsInlined
    @deprecated("Use _: Inlined", "")
    def unapply(x: Inlined): Some[Inlined] = Some(x)

  object Inlined {

    def apply(call: Option[Tree /* Term | TypeTree */], bindings: List[Definition], expansion: Term)(given ctx: Context): Inlined =
      internal.Inlined_apply(call, bindings, expansion)

    def copy(original: Tree)(call: Option[Tree /* Term | TypeTree */], bindings: List[Definition], expansion: Term)(given ctx: Context): Inlined =
      internal.Inlined_copy(original)(call, bindings, expansion)

    def unapply(x: Inlined)(given ctx: Context): Option[(Option[Tree /* Term | TypeTree */], List[Definition], Term)] =
      Some((x.call, x.bindings, x.body))

  }

  given InlinedOps: extension (self: Inlined) {
    def call(given ctx: Context): Option[Tree /* Term | TypeTree */] = internal.Inlined_call(self)
    def bindings(given ctx: Context): List[Definition] = internal.Inlined_bindings(self)
    def body(given ctx: Context): Term = internal.Inlined_body(self)
  }

  given (given Context): IsInstanceOf[SelectOuter] = internal.isInstanceOfSelectOuter

  object IsSelectOuter
    @deprecated("Use _: SelectOuter", "")
    def unapply(x: SelectOuter): Some[SelectOuter] = Some(x)

  object SelectOuter {

    def apply(qualifier: Term, name: String, levels: Int)(given ctx: Context): SelectOuter =
      internal.SelectOuter_apply(qualifier, name, levels)

    def copy(original: Tree)(qualifier: Term, name: String, levels: Int)(given ctx: Context): SelectOuter =
      internal.SelectOuter_copy(original)(qualifier, name, levels)

    def unapply(x: SelectOuter)(given ctx: Context): Option[(Term, Int, Type)] = // TODO homogenize order of parameters
      Some((x.qualifier, x.level, x.tpe))

  }

  given SelectOuterOps: extension (self: SelectOuter) {
    def qualifier(given ctx: Context): Term = internal.SelectOuter_qualifier(self)
    def level(given ctx: Context): Int = internal.SelectOuter_level(self)
  }

  given (given Context): IsInstanceOf[While] = internal.isInstanceOfWhile

  object IsWhile
    @deprecated("Use _: While", "")
    def unapply(x: While): Some[While] = Some(x)

  object While {

    /** Creates a while loop `while (<cond>) <body>` and returns (<cond>, <body>) */
    def apply(cond: Term, body: Term)(given ctx: Context): While =
      internal.While_apply(cond, body)

    def copy(original: Tree)(cond: Term, body: Term)(given ctx: Context): While =
      internal.While_copy(original)(cond, body)

    /** Extractor for while loops. Matches `while (<cond>) <body>` and returns (<cond>, <body>) */
    def unapply(x: While)(given ctx: Context): Option[(Term, Term)] =
      Some((x.cond, x.body))

  }

  given WhileOps: extension (self: While) {
    def cond(given ctx: Context): Term = internal.While_cond(self)
    def body(given ctx: Context): Term = internal.While_body(self)
  }

  // ----- TypeTrees ------------------------------------------------

  given TypeTreeOps: extension (self: TypeTree) {
    /** Type of this type tree */
    def tpe(given ctx: Context): Type = internal.TypeTree_tpe(self)
  }

  given (given Context): IsInstanceOf[TypeTree] =
    internal.isInstanceOfTypeTree

  object IsTypeTree
    @deprecated("Use _: TypeTree", "")
    def unapply(x: TypeTree): Option[TypeTree] = Some(x)

  given (given Context): IsInstanceOf[Inferred] = internal.isInstanceOfInferred

  /** TypeTree containing an inferred type */
  object IsInferred
    @deprecated("Use _: Inferred", "")
    def unapply(x: Inferred): Some[Inferred] = Some(x)

  object Inferred {
    def apply(tpe: Type)(given ctx: Context): Inferred =
      internal.Inferred_apply(tpe)
    /** Matches a TypeTree containing an inferred type */
    def unapply(x: Inferred)(given ctx: Context): Boolean = true
  }

  given (given Context): IsInstanceOf[TypeIdent] = internal.isInstanceOfTypeIdent

  given TypeIdentOps: extension (self: TypeIdent) {
    def name(given ctx: Context): String = internal.TypeIdent_name(self)
  }

  object IsTypeIdent
    @deprecated("Use _: TypeIdent", "")
    def unapply(x: TypeIdent): Some[TypeIdent] = Some(x)

  object TypeIdent {
    def apply(sym: Symbol)(given ctx: Context): TypeTree =
      internal.TypeRef_apply(sym)
    def copy(original: Tree)(name: String)(given ctx: Context): TypeIdent =
      internal.TypeIdent_copy(original)(name)
    def unapply(x: TypeIdent)(given ctx: Context): Option[String] = Some(x.name)
  }

  given (given Context): IsInstanceOf[TypeSelect] = internal.isInstanceOfTypeSelect

  object IsTypeSelect
    @deprecated("Use _: TypeSelect", "")
    def unapply(x: TypeSelect): Some[TypeSelect] = Some(x)

  object TypeSelect {
    def apply(qualifier: Term, name: String)(given ctx: Context): TypeSelect =
      internal.TypeSelect_apply(qualifier, name)
    def copy(original: Tree)(qualifier: Term, name: String)(given ctx: Context): TypeSelect =
      internal.TypeSelect_copy(original)(qualifier, name)
    def unapply(x: TypeSelect)(given ctx: Context): Option[(Term, String)] =
      Some((x.qualifier, x.name))
  }

  given TypeSelectOps: extension (self: TypeSelect) {
    def qualifier(given ctx: Context): Term = internal.TypeSelect_qualifier(self)
    def name(given ctx: Context): String = internal.TypeSelect_name(self)
  }

  given (given Context): IsInstanceOf[Projection] = internal.isInstanceOfProjection

  object IsProjection
    @deprecated("Use _: Projection", "")
    def unapply(x: Projection): Some[Projection] = Some(x)

  object Projection {
    // TODO def apply(qualifier: TypeTree, name: String)(given ctx: Context): Project
    def copy(original: Tree)(qualifier: TypeTree, name: String)(given ctx: Context): Projection =
      internal.Projection_copy(original)(qualifier, name)
    def unapply(x: Projection)(given ctx: Context): Option[(TypeTree, String)] =
      Some((x.qualifier, x.name))
  }

  given ProjectionOps: extension (self: Projection) {
    def qualifier(given ctx: Context): TypeTree = internal.Projection_qualifier(self)
    def name(given ctx: Context): String = internal.Projection_name(self)
  }

  given (given Context): IsInstanceOf[Singleton] =
    internal.isInstanceOfSingleton

  object IsSingleton
    @deprecated("Use _: Singleton", "")
    def unapply(x: Singleton): Some[Singleton] = Some(x)

  object Singleton {
    def apply(ref: Term)(given ctx: Context): Singleton =
      internal.Singleton_apply(ref)
    def copy(original: Tree)(ref: Term)(given ctx: Context): Singleton =
      internal.Singleton_copy(original)(ref)
    def unapply(x: Singleton)(given ctx: Context): Option[Term] =
      Some(x.ref)
  }

  given SingletonOps: extension (self: Singleton) {
    def ref(given ctx: Context): Term = internal.Singleton_ref(self)
  }

  given (given Context): IsInstanceOf[Refined] = internal.isInstanceOfRefined

  object IsRefined
    @deprecated("Use _: Refined", "")
    def unapply(x: Refined): Some[Refined] = Some(x)

  object Refined {
    // TODO def apply(tpt: TypeTree, refinements: List[Definition])(given ctx: Context): Refined
    def copy(original: Tree)(tpt: TypeTree, refinements: List[Definition])(given ctx: Context): Refined =
      internal.Refined_copy(original)(tpt, refinements)
    def unapply(x: Refined)(given ctx: Context): Option[(TypeTree, List[Definition])] =
      Some((x.tpt, x.refinements))
  }

  given RefinedOps: extension (self: Refined) {
    def tpt(given ctx: Context): TypeTree = internal.Refined_tpt(self)
    def refinements(given ctx: Context): List[Definition] = internal.Refined_refinements(self)
  }

  given (given Context): IsInstanceOf[Applied] = internal.isInstanceOfApplied

  object IsApplied
    @deprecated("Use _: Applied", "")
    def unapply(x: Applied): Some[Applied] = Some(x)

  object Applied {
    def apply(tpt: TypeTree, args: List[Tree /*TypeTree | TypeBoundsTree*/])(given ctx: Context): Applied =
      internal.Applied_apply(tpt, args)
    def copy(original: Tree)(tpt: TypeTree, args: List[Tree /*TypeTree | TypeBoundsTree*/])(given ctx: Context): Applied =
      internal.Applied_copy(original)(tpt, args)
    def unapply(x: Applied)(given ctx: Context): Option[(TypeTree, List[Tree /*TypeTree | TypeBoundsTree*/])] =
      Some((x.tpt, x.args))
  }

  given AppliedOps: extension (self: Applied) {
    def tpt(given ctx: Context): TypeTree = internal.Applied_tpt(self)
    def args(given ctx: Context): List[Tree /*TypeTree | TypeBoundsTree*/] = internal.Applied_args(self)
  }

  given (given Context): IsInstanceOf[Annotated] =
    internal.isInstanceOfAnnotated

  object IsAnnotated
    @deprecated("Use _: Annotated", "")
    def unapply(x: Annotated): Some[Annotated] = Some(x)

  object Annotated {
    def apply(arg: TypeTree, annotation: Term)(given ctx: Context): Annotated =
      internal.Annotated_apply(arg, annotation)
    def copy(original: Tree)(arg: TypeTree, annotation: Term)(given ctx: Context): Annotated =
      internal.Annotated_copy(original)(arg, annotation)
    def unapply(x: Annotated)(given ctx: Context): Option[(TypeTree, Term)] =
      Some((x.arg, x.annotation))
  }

  given AnnotatedOps: extension (self: Annotated) {
    def arg(given ctx: Context): TypeTree = internal.Annotated_arg(self)
    def annotation(given ctx: Context): Term = internal.Annotated_annotation(self)
  }

  given (given Context): IsInstanceOf[MatchTypeTree] =
    internal.isInstanceOfMatchTypeTree

  object IsMatchTypeTree
    @deprecated("Use _: MatchTypeTree", "")
    def unapply(x: MatchTypeTree): Some[MatchTypeTree] = Some(x)

  object MatchTypeTree {
    def apply(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef])(given ctx: Context): MatchTypeTree =
      internal.MatchTypeTree_apply(bound, selector, cases)
    def copy(original: Tree)(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef])(given ctx: Context): MatchTypeTree =
      internal.MatchTypeTree_copy(original)(bound, selector, cases)
    def unapply(x: MatchTypeTree)(given ctx: Context): Option[(Option[TypeTree], TypeTree, List[TypeCaseDef])] =
      Some((x.bound, x.selector, x.cases))
  }

  given MatchTypeTreeOps: extension (self: MatchTypeTree) {
    def bound(given ctx: Context): Option[TypeTree] = internal.MatchTypeTree_bound(self)
    def selector(given ctx: Context): TypeTree = internal.MatchTypeTree_selector(self)
    def cases(given ctx: Context): List[TypeCaseDef] = internal.MatchTypeTree_cases(self)
  }

  given (given Context): IsInstanceOf[ByName] =
    internal.isInstanceOfByName

  object IsByName
    @deprecated("Use _: ByName", "")
    def unapply(x: ByName): Some[ByName] = Some(x)

  object ByName {
    def apply(result: TypeTree)(given ctx: Context): ByName =
      internal.ByName_apply(result)
    def copy(original: Tree)(result: TypeTree)(given ctx: Context): ByName =
      internal.ByName_copy(original)(result)
    def unapply(x: ByName)(given ctx: Context): Option[TypeTree] =
      Some(x.result)
  }

  given ByNameOps: extension (self: ByName) {
    def result(given ctx: Context): TypeTree = internal.ByName_result(self)
  }

  given (given Context): IsInstanceOf[LambdaTypeTree] = internal.isInstanceOfLambdaTypeTree

  object IsLambdaTypeTree
    @deprecated("Use _: LambdaTypeTree", "")
    def unapply(x: LambdaTypeTree): Some[LambdaTypeTree] = Some(x)

  object LambdaTypeTree {
    def apply(tparams: List[TypeDef], body: Tree /*TypeTree | TypeBoundsTree*/)(given ctx: Context): LambdaTypeTree =
      internal.Lambdaapply(tparams, body)
    def copy(original: Tree)(tparams: List[TypeDef], body: Tree /*TypeTree | TypeBoundsTree*/)(given ctx: Context): LambdaTypeTree =
      internal.Lambdacopy(original)(tparams, body)
    def unapply(tree: LambdaTypeTree)(given ctx: Context): Option[(List[TypeDef], Tree /*TypeTree | TypeBoundsTree*/)] =
      Some((tree.tparams, tree.body))
  }

  given LambdaTypeTreeOps: extension (self: LambdaTypeTree) {
    def tparams(given ctx: Context): List[TypeDef] = internal.Lambdatparams(self)
    def body(given ctx: Context): Tree /*TypeTree | TypeBoundsTree*/ = internal.Lambdabody(self)
  }

  given (given Context): IsInstanceOf[TypeBind] = internal.isInstanceOfTypeBind

  object IsTypeBind
    @deprecated("Use _: TypeBind", "")
    def unapply(x: TypeBind): Some[TypeBind] = Some(x)

  object TypeBind {
    // TODO def apply(name: String, tree: Tree)(given ctx: Context): TypeBind
    def copy(original: Tree)(name: String, tpt: Tree /*TypeTree | TypeBoundsTree*/)(given ctx: Context): TypeBind =
      internal.TypeBind_copy(original)(name, tpt)
    def unapply(x: TypeBind)(given ctx: Context): Option[(String, Tree /*TypeTree | TypeBoundsTree*/)] =
      Some((x.name, x.body))
  }

  given TypeBindOps: extension (self: TypeBind) {
    def name(given ctx: Context): String = internal.TypeBind_name(self)
    def body(given ctx: Context): Tree /*TypeTree | TypeBoundsTree*/ = internal.TypeBind_body(self)
  }

  given (given Context): IsInstanceOf[TypeBlock] = internal.isInstanceOfTypeBlock

  object IsTypeBlock
    @deprecated("Use _: TypeBlock", "")
    def unapply(x: TypeBlock): Some[TypeBlock] = Some(x)

  object TypeBlock {
    def apply(aliases: List[TypeDef], tpt: TypeTree)(given ctx: Context): TypeBlock =
      internal.TypeBlock_apply(aliases, tpt)
    def copy(original: Tree)(aliases: List[TypeDef], tpt: TypeTree)(given ctx: Context): TypeBlock =
      internal.TypeBlock_copy(original)(aliases, tpt)
    def unapply(x: TypeBlock)(given ctx: Context): Option[(List[TypeDef], TypeTree)] =
      Some((x.aliases, x.tpt))
  }

  given TypeBlockOps: extension (self: TypeBlock) {
    def aliases(given ctx: Context): List[TypeDef] = internal.TypeBlock_aliases(self)
    def tpt(given ctx: Context): TypeTree = internal.TypeBlock_tpt(self)
  }

  // ----- TypeBoundsTrees ------------------------------------------------

  given TypeBoundsTreeOps: extension (self: TypeBoundsTree) {
    def tpe(given ctx: Context): TypeBounds = internal.TypeBoundsTree_tpe(self)
    def low(given ctx: Context): TypeTree = internal.TypeBoundsTree_low(self)
    def hi(given ctx: Context): TypeTree = internal.TypeBoundsTree_hi(self)
  }

  given (given Context): IsInstanceOf[TypeBoundsTree] = internal.isInstanceOfTypeBoundsTree

  object IsTypeBoundsTree
    @deprecated("Use _: TypeBoundsTree", "")
    def unapply(x: TypeBoundsTree): Some[TypeBoundsTree] = Some(x)

  object TypeBoundsTree {
    def unapply(x: TypeBoundsTree)(given ctx: Context): Option[(TypeTree, TypeTree)] =
      Some((x.low, x.hi))
  }

  given WildcardTypeTreeOps: extension (self: WildcardTypeTree) {
    def tpe(given ctx: Context): TypeOrBounds = internal.WildcardTypeTree_tpe(self)
  }

  given (given Context): IsInstanceOf[WildcardTypeTree] = internal.isInstanceOfWildcardTypeTree

  /** TypeBoundsTree containing wildcard type bounds */
  object IsWildcardTypeTree
    @deprecated("Use _: WildcardTypeTree", "")
    def unapply(x: WildcardTypeTree): Some[WildcardTypeTree] = Some(x)

  object WildcardTypeTree {
    /** Matches a TypeBoundsTree containing wildcard type bounds */
    def unapply(x: WildcardTypeTree)(given ctx: Context): Boolean = true
  }

  // ----- CaseDefs ------------------------------------------------

  given CaseDefOps: extension (caseDef: CaseDef) {
    def pattern(given ctx: Context): Tree = internal.CaseDef_pattern(caseDef)
    def guard(given ctx: Context): Option[Term] = internal.CaseDef_guard(caseDef)
    def rhs(given ctx: Context): Term = internal.CaseDef_rhs(caseDef)
  }

  given (given Context): IsInstanceOf[CaseDef] = internal.isInstanceOfCaseDef

  object IsCaseDef
    @deprecated("Use _: CaseDef", "")
    def unapply(x: CaseDef): Some[CaseDef] = Some(x)

  object CaseDef {
    def apply(pattern: Tree, guard: Option[Term], rhs: Term)(given ctx: Context): CaseDef =
      internal.CaseDef_module_apply(pattern, guard, rhs)

    def copy(original: Tree)(pattern: Tree, guard: Option[Term], rhs: Term)(given ctx: Context): CaseDef =
      internal.CaseDef_module_copy(original)(pattern, guard, rhs)

    def unapply(x: CaseDef)(given ctx: Context): Option[(Tree, Option[Term], Term)] =
      Some((x.pattern, x.guard, x.rhs))
  }

  given TypeCaseDefOps: extension (caseDef: TypeCaseDef) {
    def pattern(given ctx: Context): TypeTree = internal.TypeCaseDef_pattern(caseDef)
    def rhs(given ctx: Context): TypeTree = internal.TypeCaseDef_rhs(caseDef)
  }

  given (given Context): IsInstanceOf[TypeCaseDef] =
    internal.isInstanceOfTypeCaseDef

  object IsTypeCaseDef
    @deprecated("Use _: TypeCaseDef", "")
    def unapply(x: TypeCaseDef): Some[TypeCaseDef] = Some(x)

  object TypeCaseDef {
    def apply(pattern: TypeTree, rhs: TypeTree)(given ctx: Context): TypeCaseDef =
      internal.TypeCaseDef_module_apply(pattern, rhs)

    def copy(original: Tree)(pattern: TypeTree, rhs: TypeTree)(given ctx: Context): TypeCaseDef =
      internal.TypeCaseDef_module_copy(original)(pattern, rhs)

    def unapply(tree: TypeCaseDef)(given ctx: Context): Option[(TypeTree, TypeTree)] =
      Some((tree.pattern, tree.rhs))
  }

  // ----- Trees ------------------------------------------------

  given (given Context): IsInstanceOf[Bind] = internal.isInstanceOfBind

  object IsBind
    @deprecated("Use _: Bind", "")
    def unapply(x: Bind): Some[Bind] = Some(x)

  object Bind {
    // TODO def apply(name: String, pattern: Tree)(given ctx: Context): Bind
    def copy(original: Tree)(name: String, pattern: Tree)(given ctx: Context): Bind =
      internal.Tree_Bind_module_copy(original)(name, pattern)
    def unapply(pattern: Bind)(given ctx: Context): Option[(String, Tree)] =
      Some((pattern.name, pattern.pattern))
  }

  given BindOps: extension (bind: Bind) {
    def name(given ctx: Context): String = internal.Tree_Bind_name(bind)
    def pattern(given ctx: Context): Tree = internal.Tree_Bind_pattern(bind)
  }

  given (given Context): IsInstanceOf[Unapply] = internal.isInstanceOfUnapply

  object IsUnapply
    @deprecated("Use _: Unapply", "")
    def unapply(x: Unapply): Some[Unapply] = Some(x)

  object Unapply {
    // TODO def apply(fun: Term, implicits: List[Term], patterns: List[Tree])(given ctx: Context): Unapply
    def copy(original: Tree)(fun: Term, implicits: List[Term], patterns: List[Tree])(given ctx: Context): Unapply =
      internal.Tree_Unapply_module_copy(original)(fun, implicits, patterns)
    def unapply(x: Unapply)(given ctx: Context): Option[(Term, List[Term], List[Tree])] =
      Some((x.fun, x.implicits, x.patterns))
  }

  given UnapplyOps: extension (unapply: Unapply) {
    def fun(given ctx: Context): Term = internal.Tree_Unapply_fun(unapply)
    def implicits(given ctx: Context): List[Term] = internal.Tree_Unapply_implicits(unapply)
    def patterns(given ctx: Context): List[Tree] = internal.Tree_Unapply_patterns(unapply)
  }

  given (given Context): IsInstanceOf[Alternatives] = internal.isInstanceOfAlternatives

  object IsAlternatives
    @deprecated("Use _: Alternatives", "")
    def unapply(x: Alternatives): Some[Alternatives] = Some(x)

  object Alternatives {
    def apply(patterns: List[Tree])(given ctx: Context): Alternatives =
      internal.Tree_Alternatives_module_apply(patterns)
    def copy(original: Tree)(patterns: List[Tree])(given ctx: Context): Alternatives =
      internal.Tree_Alternatives_module_copy(original)(patterns)
    def unapply(x: Alternatives)(given ctx: Context): Option[List[Tree]] =
      Some(x.patterns)
  }

  given AlternativesOps: extension (alternatives: Alternatives) {
    def patterns(given ctx: Context): List[Tree] = internal.Tree_Alternatives_patterns(alternatives)
  }


}
