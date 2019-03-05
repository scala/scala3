package scala.tasty
package reflect

trait TreeOps extends Core {

  // Decorators

  implicit def termAsTermOrTypeTree(term: Term): TermOrTypeTree = term.asInstanceOf[TermOrTypeTree]

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
    def apply(pid: Term.Ref, stats: List[Tree])(implicit ctx: Context): PackageClause =
      kernel.PackageClause_apply(pid, stats)
    def copy(original: PackageClause)(pid: Term.Ref, stats: List[Tree])(implicit ctx: Context): PackageClause =
      kernel.PackageClause_copy(original)(pid, stats)
    def unapply(tree: Tree)(implicit ctx: Context): Option[(Term.Ref, List[Tree])] =
      kernel.matchPackageClause(tree).map(x => (x.pid, x.stats))
  }

  implicit class PackageClauseAPI(self: PackageClause) {
    def pid(implicit ctx: Context): Term.Ref = kernel.PackageClause_pid(self)
    def stats(implicit ctx: Context): List[Tree] = kernel.PackageClause_stats(self)
  }

  object IsImport {
    def unapply(tree: Tree)(implicit ctx: Context): Option[Import] =
      kernel.matchImport(tree)
  }

  object Import {
    def apply(impliedOnly: Boolean, expr: Term, selectors: List[ImportSelector])(implicit ctx: Context): Import =
      kernel.Import_apply(impliedOnly, expr, selectors)
    def copy(original: Import)(impliedOnly: Boolean, expr: Term, selectors: List[ImportSelector])(implicit ctx: Context): Import =
      kernel.Import_copy(original)(impliedOnly, expr, selectors)
    def unapply(tree: Tree)(implicit ctx: Context): Option[(Boolean, Term, List[ImportSelector])] =
      kernel.matchImport(tree).map(x => (x.impliedOnly, x.expr, x.selectors))
  }

  implicit class ImportAPI(self: Import)  {
    def impliedOnly: Boolean = kernel.Import_impliedOnly(self)
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
    def copy(original: ClassDef)(name: String, constr: DefDef, parents: List[TermOrTypeTree], derived: List[TypeTree], selfOpt: Option[ValDef], body: List[Statement])(implicit ctx: Context): ClassDef =
      kernel.ClassDef_copy(original)(name, constr, parents, derived, selfOpt, body)
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, DefDef, List[TermOrTypeTree], List[TypeTree], Option[ValDef], List[Statement])] =
      kernel.matchClassDef(tree).map(x => (x.name, x.constructor, x.parents, x.derived, x.self, x.body))
  }

  implicit class ClassDefAPI(self: ClassDef) {
    def constructor(implicit ctx: Context): DefDef = kernel.ClassDef_constructor(self)
    def parents(implicit ctx: Context): List[TermOrTypeTree] = kernel.ClassDef_parents(self)
    def derived(implicit ctx: Context): List[TypeTree] = kernel.ClassDef_derived(self)
    def self(implicit ctx: Context): Option[ValDef] = kernel.ClassDef_self(self)
    def body(implicit ctx: Context): List[Statement] = kernel.ClassDef_body(self)
    def symbol(implicit ctx: Context): ClassSymbol = kernel.ClassDef_symbol(self)
  }

  // DefDef

  object IsDefDef {
    def unapply(tree: Tree)(implicit ctx: Context): Option[DefDef] = kernel.matchDefDef(tree)
  }

  object DefDef {
    def apply(symbol: DefSymbol, rhsFn: List[Type] => List[List[Term]] => Option[Term])(implicit ctx: Context): DefDef =
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
    def symbol(implicit ctx: Context): DefSymbol = kernel.DefDef_symbol(self)
  }

  // ValDef

  object IsValDef {
    def unapply(tree: Tree)(implicit ctx: Context): Option[ValDef] = kernel.matchValDef(tree)
  }

  object ValDef {
    def apply(symbol: ValSymbol, rhs: Option[Term])(implicit ctx: Context): ValDef =
      kernel.ValDef_apply(symbol, rhs)
    def copy(original: ValDef)(name: String, tpt: TypeTree, rhs: Option[Term])(implicit ctx: Context): ValDef =
      kernel.ValDef_copy(original)(name, tpt, rhs)
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, TypeTree, Option[Term])] =
      kernel.matchValDef(tree).map(x => (x.name, x.tpt, x.rhs))
  }

  implicit class ValDefAPI(self: ValDef) {
    def tpt(implicit ctx: Context): TypeTree = kernel.ValDef_tpt(self)
    def rhs(implicit ctx: Context): Option[Term] = kernel.ValDef_rhs(self)
    def symbol(implicit ctx: Context): ValSymbol = kernel.ValDef_symbol(self)
  }

  // TypeDef

  object IsTypeDef {
    def unapply(tree: Tree)(implicit ctx: Context): Option[TypeDef] = kernel.matchTypeDef(tree)
  }

  object TypeDef {
    def apply(symbol: TypeSymbol)(implicit ctx: Context): TypeDef =
      kernel.TypeDef_apply(symbol)
    def copy(original: TypeDef)(name: String, rhs: TypeOrBoundsTree)(implicit ctx: Context): TypeDef =
      kernel.TypeDef_copy(original)(name, rhs)
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, TypeOrBoundsTree /* TypeTree | TypeBoundsTree */)] =
      kernel.matchTypeDef(tree).map(x => (x.name, x.rhs))
  }

  implicit class TypeDefAPI(self: TypeDef) {
    def rhs(implicit ctx: Context): TypeOrBoundsTree = kernel.TypeDef_rhs(self)
    def symbol(implicit ctx: Context): TypeSymbol = kernel.TypeDef_symbol(self)
  }

  // PackageDef

  object IsPackageDef {
    def unapply(tree: Tree)(implicit ctx: Context): Option[PackageDef] =
      kernel.matchPackageDef(tree)
  }

  implicit class PackageDefAPI(self: PackageDef) {
    def owner(implicit ctx: Context): PackageDef = kernel.PackageDef_owner(self)
    def members(implicit ctx: Context): List[Statement] = kernel.PackageDef_members(self)
    def symbol(implicit ctx: Context): PackageSymbol = kernel.PackageDef_symbol(self)
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
  }

  object IsTerm {
    /** Matches any term */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Term] =
      kernel.matchTerm(tree)

    /** Matches any term */
    def unapply(parent: TermOrTypeTree)(implicit ctx: Context, dummy: DummyImplicit): Option[Term] =
      kernel.matchTermNotTypeTree(parent)
  }

  /** Scala term. Any tree that can go in expression position. */
  object Term extends TermCoreModule {

    object IsIdent {
      /** Matches any Ident and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Ident] = kernel.matchIdent(tree)
    }

    object Ref {

      /** Create a reference tree */
      def apply(sym: Symbol)(implicit ctx: Context): Ref =
        kernel.Ref_apply(sym)

      // TODO def copy(original: Tree)(name: String)(implicit ctx: Context): Ref

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

    object IsThis {
      /** Matches any This and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[This] = kernel.matchThis(tree)
    }

    /** Scala `this` or `this[id]` */
    object This {

      /** Create a `this[<id: Id]>` */
      def apply(cls: ClassSymbol)(implicit ctx: Context): This =
        kernel.This_apply(cls)

      def copy(original: Tree)(qual: Option[Id])(implicit ctx: Context): This =
        kernel.This_copy(original)(qual)

      /** Matches `this[<id: Option[Id]>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Option[Id]] =
        kernel.matchThis(tree).map(_.id)

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

    object IsInlined {
      /** Matches any Inlined and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Inlined] = kernel.matchInlined(tree)
    }

    object Inlined {

      def apply(call: Option[TermOrTypeTree], bindings: List[Definition], expansion: Term)(implicit ctx: Context): Inlined =
        kernel.Inlined_apply(call, bindings, expansion)

      def copy(original: Tree)(call: Option[TermOrTypeTree], bindings: List[Definition], expansion: Term)(implicit ctx: Context): Inlined =
        kernel.Inlined_copy(original)(call, bindings, expansion)

      def unapply(tree: Tree)(implicit ctx: Context): Option[(Option[TermOrTypeTree], List[Definition], Term)] =
        kernel.matchInlined(tree).map(x => (x.call, x.bindings, x.body))

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
  }

  implicit class IdentAPI(self: Term.Ident) {
    def name(implicit ctx: Context): String = kernel.Ident_name(self)
  }

  implicit class SelectAPI(self: Term.Select) {
    def qualifier(implicit ctx: Context): Term = kernel.Select_qualifier(self)
    def name(implicit ctx: Context): String = kernel.Select_name(self)
    def signature(implicit ctx: Context): Option[Signature] = kernel.Select_signature(self)
  }

  implicit class LiteralAPI(self: Term.Literal) {
    def constant(implicit ctx: Context): Constant = kernel.Literal_constant(self)
  }

  implicit class ThisAPI(self: Term.This) {
    def id(implicit ctx: Context): Option[Id] = kernel.This_id(self)
  }

  implicit class NewAPI(self: Term.New) {
    def tpt(implicit ctx: Context): TypeTree = kernel.New_tpt(self)
  }

  implicit class NamedArgAPI(self: Term.NamedArg) {
    def name(implicit ctx: Context): String = kernel.NamedArg_name(self)
    def value(implicit ctx: Context): Term = kernel.NamedArg_value(self)
  }

  implicit class ApplyAPI(self: Term.Apply) {
    def fun(implicit ctx: Context): Term = kernel.Apply_fun(self)
    def args(implicit ctx: Context): List[Term] = kernel.Apply_args(self)
  }

  implicit class TypeApplyAPI(self: Term.TypeApply) {
    def fun(implicit ctx: Context): Term = kernel.TypeApply_fun(self)
    def args(implicit ctx: Context): List[TypeTree] = kernel.TypeApply_args(self)
  }

  implicit class SuperAPI(self: Term.Super) {
    def qualifier(implicit ctx: Context): Term = kernel.Super_qualifier(self)
    def id(implicit ctx: Context): Option[Id] = kernel.Super_id(self)
  }

  implicit class TypedAPI(self: Term.Typed) {
    def expr(implicit ctx: Context): Term = kernel.Typed_expr(self)
    def tpt(implicit ctx: Context): TypeTree = kernel.Typed_tpt(self)
  }

  implicit class AssignAPI(self: Term.Assign) {
    def lhs(implicit ctx: Context): Term = kernel.Assign_lhs(self)
    def rhs(implicit ctx: Context): Term = kernel.Assign_rhs(self)
  }

  implicit class BlockAPI(self: Term.Block) {
    def statements(implicit ctx: Context): List[Statement] = kernel.Block_statements(self)
    def expr(implicit ctx: Context): Term = kernel.Block_expr(self)
  }

  implicit class LambdaAPI(self: Term.Lambda) {
    def meth(implicit ctx: Context): Term = kernel.Lambda_meth(self)
    def tptOpt(implicit ctx: Context): Option[TypeTree] = kernel.Lambda_tptOpt(self)
  }

  implicit class IfAPI(self: Term.If) {
    def cond(implicit ctx: Context): Term = kernel.If_cond(self)
    def thenp(implicit ctx: Context): Term = kernel.If_thenp(self)
    def elsep(implicit ctx: Context): Term = kernel.If_elsep(self)
  }

  implicit class MatchAPI(self: Term.Match) {
    def scrutinee(implicit ctx: Context): Term = kernel.Match_scrutinee(self)
    def cases(implicit ctx: Context): List[CaseDef] = kernel.Match_cases(self)
  }

  implicit class TryAPI(self: Term.Try) {
    def body(implicit ctx: Context): Term = kernel.Try_body(self)
    def cases(implicit ctx: Context): List[CaseDef] = kernel.Try_cases(self)
    def finalizer(implicit ctx: Context): Option[Term] = kernel.Try_finalizer(self)
  }

  implicit class ReturnAPI(self: Term.Return) {
    def expr(implicit ctx: Context): Term = kernel.Return_expr(self)
  }

  implicit class RepeatedAPI(self: Term.Repeated) {
    def elems(implicit ctx: Context): List[Term] = kernel.Repeated_elems(self)
    def elemtpt(implicit ctx: Context): TypeTree = kernel.Repeated_elemtpt(self)
  }

  implicit class InlinedAPI(self: Term.Inlined) {
    def call(implicit ctx: Context): Option[TermOrTypeTree] = kernel.Inlined_call(self)
    def bindings(implicit ctx: Context): List[Definition] = kernel.Inlined_bindings(self)
    def body(implicit ctx: Context): Term = kernel.Inlined_body(self)
  }

  implicit class SelectOuterAPI(self: Term.SelectOuter) {
    def qualifier(implicit ctx: Context): Term = kernel.SelectOuter_qualifier(self)
    def level(implicit ctx: Context): Int = kernel.SelectOuter_level(self)
    def tpe(implicit ctx: Context): Type = kernel.SelectOuter_tpe(self)
  }

  implicit class WhileAPI(self: Term.While) {
    def cond(implicit ctx: Context): Term = kernel.While_cond(self)
    def body(implicit ctx: Context): Term = kernel.While_body(self)
  }

}
