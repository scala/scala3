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
  }

  object IsTerm {
    /** Matches any term */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Term] =
      kernel.matchTerm(tree)
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
      def apply(cls: ClassDefSymbol)(implicit ctx: Context): This =
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

    object IsImplicitMatch {
      /** Matches any ImplicitMatch and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[ImplicitMatch] = kernel.matchImplicitMatch(tree)
    }

    /** Scala implicit `match` term */
    object ImplicitMatch {

      /** Creates a pattern match `implicit match { <cases: List[CaseDef]> }` */
      def apply(cases: List[CaseDef])(implicit ctx: Context): ImplicitMatch =
        kernel.ImplicitMatch_apply(cases)

      def copy(original: Tree)(cases: List[CaseDef])(implicit ctx: Context): ImplicitMatch =
        kernel.ImplicitMatch_copy(original)(cases)

      /** Matches a pattern match `implicit match { <cases: List[CaseDef]> }` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[List[CaseDef]] =
        kernel.matchImplicitMatch(tree).map(_.cases)

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

      def apply(call: Option[Tree /* Term | TypeTree */], bindings: List[Definition], expansion: Term)(implicit ctx: Context): Inlined =
        kernel.Inlined_apply(call, bindings, expansion)

      def copy(original: Tree)(call: Option[Tree /* Term | TypeTree */], bindings: List[Definition], expansion: Term)(implicit ctx: Context): Inlined =
        kernel.Inlined_copy(original)(call, bindings, expansion)

      def unapply(tree: Tree)(implicit ctx: Context): Option[(Option[Tree /* Term | TypeTree */], List[Definition], Term)] =
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

  implicit class ImplicitMatchAPI(self: Term.ImplicitMatch) {
    def cases(implicit ctx: Context): List[CaseDef] = kernel.ImplicitMatch_cases(self)
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
    def call(implicit ctx: Context): Option[Tree /* Term | TypeTree */] = kernel.Inlined_call(self)
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

  object TypeTree extends TypeTreeCoreModule {

    object IsInferred {
      /** Matches any Inferred and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Inferred] =
        kernel.matchTypeTree_Inferred(tree)
    }

    /** TypeTree containing an inferred type */
    object Inferred {
      def apply(tpe: Type)(implicit ctx: Context): Inferred =
        kernel.TypeTree_Inferred_apply(tpe)
      /** Matches a TypeTree containing an inferred type */
      def unapply(tree: Tree)(implicit ctx: Context): Boolean =
        kernel.matchTypeTree_Inferred(tree).isDefined
    }

    object IsIdent {
      /** Matches any Ident and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Ident] =
        kernel.matchTypeTree_Ident(tree)
    }

    object Ident {
      // TODO def apply(name: String)(implicit ctx: Context): Ident
      def copy(original: Ident)(name: String)(implicit ctx: Context): Ident =
        kernel.TypeTree_Ident_copy(original)(name)
      def unapply(tree: Tree)(implicit ctx: Context): Option[String] =
        kernel.matchTypeTree_Ident(tree).map(_.name)
    }

    object IsSelect {
      /** Matches any Select and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Select] =
        kernel.matchTypeTree_Select(tree)
    }

    object Select {
      def apply(qualifier: Term, name: String)(implicit ctx: Context): Select =
        kernel.TypeTree_Select_apply(qualifier, name)
      def copy(original: Select)(qualifier: Term, name: String)(implicit ctx: Context): Select =
        kernel.TypeTree_Select_copy(original)(qualifier, name)
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, String)] =
        kernel.matchTypeTree_Select(tree).map(x => (x.qualifier, x.name))
    }

    object IsProjection {
      /** Matches any Projection and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Projection] =
        kernel.matchTypeTree_Projection(tree)
    }

    object Projection {
      // TODO def apply(qualifier: TypeTree, name: String)(implicit ctx: Context): Project
      def copy(original: Projection)(qualifier: TypeTree, name: String)(implicit ctx: Context): Projection =
        kernel.TypeTree_Projection_copy(original)(qualifier, name)
      def unapply(tree: Tree)(implicit ctx: Context): Option[(TypeTree, String)] =
        kernel.matchTypeTree_Projection(tree).map(x => (x.qualifier, x.name))
    }

    object IsSingleton {
      /** Matches any Singleton and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Singleton] =
        kernel.matchTypeTree_Singleton(tree)
    }

    object Singleton {
      def apply(ref: Term)(implicit ctx: Context): Singleton =
        kernel.TypeTree_Singleton_apply(ref)
      def copy(original: Singleton)(ref: Term)(implicit ctx: Context): Singleton =
        kernel.TypeTree_Singleton_copy(original)(ref)
      def unapply(tree: Tree)(implicit ctx: Context): Option[Term] =
        kernel.matchTypeTree_Singleton(tree).map(_.ref)
    }

    object IsRefined {
      /** Matches any Refined and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Refined] =
        kernel.matchTypeTree_Refined(tree)
    }

    object Refined {
      // TODO def apply(tpt: TypeTree, refinements: List[Definition])(implicit ctx: Context): Refined
      def copy(original: Refined)(tpt: TypeTree, refinements: List[Definition])(implicit ctx: Context): Refined =
        kernel.TypeTree_Refined_copy(original)(tpt, refinements)
      def unapply(tree: Tree)(implicit ctx: Context): Option[(TypeTree, List[Definition])] =
        kernel.matchTypeTree_Refined(tree).map(x => (x.tpt, x.refinements))
    }

    object IsApplied {
      /** Matches any Applied and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Applied] =
        kernel.matchTypeTree_Applied(tree)
    }

    object Applied {
      def apply(tpt: TypeTree, args: List[Tree /*TypeTree | TypeBoundsTree*/])(implicit ctx: Context): Applied =
        kernel.TypeTree_Applied_apply(tpt, args)
      def copy(original: Applied)(tpt: TypeTree, args: List[Tree /*TypeTree | TypeBoundsTree*/])(implicit ctx: Context): Applied =
        kernel.TypeTree_Applied_copy(original)(tpt, args)
      def unapply(tree: Tree)(implicit ctx: Context): Option[(TypeTree, List[Tree /*TypeTree | TypeBoundsTree*/])] =
        kernel.matchTypeTree_Applied(tree).map(x => (x.tpt, x.args))
    }

    object IsAnnotated {
      /** Matches any Annotated and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Annotated] =
        kernel.matchTypeTree_Annotated(tree)
    }

    object Annotated {
      def apply(arg: TypeTree, annotation: Term)(implicit ctx: Context): Annotated =
        kernel.TypeTree_Annotated_apply(arg, annotation)
      def copy(original: Annotated)(arg: TypeTree, annotation: Term)(implicit ctx: Context): Annotated =
        kernel.TypeTree_Annotated_copy(original)(arg, annotation)
      def unapply(tree: Tree)(implicit ctx: Context): Option[(TypeTree, Term)] =
        kernel.matchTypeTree_Annotated(tree).map(x => (x.arg, x.annotation))
    }

    object IsMatchType {
      /** Matches any MatchType and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[MatchType] =
        kernel.matchTypeTree_MatchType(tree)
    }

    object MatchType {
      def apply(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef])(implicit ctx: Context): MatchType =
        kernel.TypeTree_MatchType_apply(bound, selector, cases)
      def copy(original: MatchType)(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef])(implicit ctx: Context): MatchType =
        kernel.TypeTree_MatchType_copy(original)(bound, selector, cases)
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Option[TypeTree], TypeTree, List[TypeCaseDef])] =
        kernel.matchTypeTree_MatchType(tree).map(x => (x.bound, x.selector, x.cases))
    }

    object IsByName {
      /** Matches any ByName and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[ByName] =
        kernel.matchTypeTree_ByName(tree)
    }

    object ByName {
      def apply(result: TypeTree)(implicit ctx: Context): ByName =
        kernel.TypeTree_ByName_apply(result)
      def copy(original: ByName)(result: TypeTree)(implicit ctx: Context): ByName =
        kernel.TypeTree_ByName_copy(original)(result)
      def unapply(tree: Tree)(implicit ctx: Context): Option[TypeTree] =
        kernel.matchTypeTree_ByName(tree).map(_.result)
    }

    object IsLambdaTypeTree {
      /** Matches any LambdaTypeTree and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[LambdaTypeTree] =
        kernel.matchTypeTree_LambdaTypeTree(tree)
    }

    object LambdaTypeTree {
      def apply(tparams: List[TypeDef], body: Tree /*TypeTree | TypeBoundsTree*/)(implicit ctx: Context): LambdaTypeTree =
        kernel.TypeTree_LambdaTypeTree_apply(tparams, body)
      def copy(original: LambdaTypeTree)(tparams: List[TypeDef], body: Tree /*TypeTree | TypeBoundsTree*/)(implicit ctx: Context): LambdaTypeTree =
        kernel.TypeTree_LambdaTypeTree_copy(original)(tparams, body)
      def unapply(tree: Tree)(implicit ctx: Context): Option[(List[TypeDef], Tree /*TypeTree | TypeBoundsTree*/)] =
        kernel.matchTypeTree_LambdaTypeTree(tree).map(x => (x.tparams, x.body))
    }

    object IsTypeBind {
      /** Matches any TypeBind and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[TypeBind] =
        kernel.matchTypeTree_TypeBind(tree)
    }

    object TypeBind {
      // TODO def apply(name: String, tree: Tree)(implicit ctx: Context): TypeBind
      def copy(original: TypeBind)(name: String, tpt: Tree /*TypeTree | TypeBoundsTree*/)(implicit ctx: Context): TypeBind =
        kernel.TypeTree_TypeBind_copy(original)(name, tpt)
      def unapply(tree: Tree)(implicit ctx: Context): Option[(String, Tree /*TypeTree | TypeBoundsTree*/)] =
        kernel.matchTypeTree_TypeBind(tree).map(x => (x.name, x.body))
    }

    object IsTypeBlock {
      /** Matches any TypeBlock and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[TypeBlock] =
        kernel.matchTypeTree_TypeBlock(tree)
    }

    object TypeBlock {
      def apply(aliases: List[TypeDef], tpt: TypeTree)(implicit ctx: Context): TypeBlock =
        kernel.TypeTree_TypeBlock_apply(aliases, tpt)
      def copy(original: TypeBlock)(aliases: List[TypeDef], tpt: TypeTree)(implicit ctx: Context): TypeBlock =
        kernel.TypeTree_TypeBlock_copy(original)(aliases, tpt)
      def unapply(tree: Tree)(implicit ctx: Context): Option[(List[TypeDef], TypeTree)] =
        kernel.matchTypeTree_TypeBlock(tree).map(x => (x.aliases, x.tpt))
    }
  }

  implicit class TypeTree_IdentAPI(self: TypeTree.Ident) {
    def name(implicit ctx: Context): String = kernel.TypeTree_Ident_name(self)
  }

  implicit class TypeTree_SelectAPI(self: TypeTree.Select) {
    def qualifier(implicit ctx: Context): Term = kernel.TypeTree_Select_qualifier(self)
    def name(implicit ctx: Context): String = kernel.TypeTree_Select_name(self)
  }

  implicit class TypeTree_ProjectionAPI(self: TypeTree.Projection) {
    def qualifier(implicit ctx: Context): TypeTree = kernel.TypeTree_Projection_qualifier(self)
    def name(implicit ctx: Context): String = kernel.TypeTree_Projection_name(self)
  }

  implicit class TypeTree_SingletonAPI(self: TypeTree.Singleton) {
    def ref(implicit ctx: Context): Term = kernel.TypeTree_Singleton_ref(self)
  }

  implicit class TypeTree_RefinedAPI(self: TypeTree.Refined) {
    def tpt(implicit ctx: Context): TypeTree = kernel.TypeTree_Refined_tpt(self)
    def refinements(implicit ctx: Context): List[Definition] = kernel.TypeTree_Refined_refinements(self)
  }

  implicit class TypeTree_AppliedAPI(self: TypeTree.Applied) {
    def tpt(implicit ctx: Context): TypeTree = kernel.TypeTree_Applied_tpt(self)
    def args(implicit ctx: Context): List[Tree /*TypeTree | TypeBoundsTree*/] = kernel.TypeTree_Applied_args(self)
  }

  implicit class TypeTree_AnnotatedAPI(self: TypeTree.Annotated) {
    def arg(implicit ctx: Context): TypeTree = kernel.TypeTree_Annotated_arg(self)
    def annotation(implicit ctx: Context): Term = kernel.TypeTree_Annotated_annotation(self)
  }

  implicit class TypeTree_MatchTypeAPI(self: TypeTree.MatchType) {
    def bound(implicit ctx: Context): Option[TypeTree] = kernel.TypeTree_MatchType_bound(self)
    def selector(implicit ctx: Context): TypeTree = kernel.TypeTree_MatchType_selector(self)
    def cases(implicit ctx: Context): List[TypeCaseDef] = kernel.TypeTree_MatchType_cases(self)
  }

  implicit class TypeTree_ByNameAPI(self: TypeTree.ByName) {
    def result(implicit ctx: Context): TypeTree = kernel.TypeTree_ByName_result(self)
  }

  implicit class TypeTree_LambdaTypeTreeAPI(self: TypeTree.LambdaTypeTree) {
    def tparams(implicit ctx: Context): List[TypeDef] = kernel.TypeTree_LambdaTypeTree_tparams(self)
    def body(implicit ctx: Context): Tree /*TypeTree | TypeBoundsTree*/ = kernel.TypeTree_LambdaTypeTree_body(self)
  }

  implicit class TypeTree_TypeBindAPI(self: TypeTree.TypeBind) {
    def name(implicit ctx: Context): String = kernel.TypeTree_TypeBind_name(self)
    def body(implicit ctx: Context): Tree /*TypeTree | TypeBoundsTree*/ = kernel.TypeTree_TypeBind_body(self)
  }

  implicit class TypeTree_TypeBlockAPI(self: TypeTree.TypeBlock) {
    def aliases(implicit ctx: Context): List[TypeDef] = kernel.TypeTree_TypeBlock_aliases(self)
    def tpt(implicit ctx: Context): TypeTree = kernel.TypeTree_TypeBlock_tpt(self)
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
