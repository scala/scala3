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
      kernel.isPackageClause(tree)
  }

  object PackageClause {
    def apply(pid: Term.Ref, stats: List[Tree])(implicit ctx: Context): PackageClause =
      kernel.PackageClause_apply(pid, stats)
    def copy(original: PackageClause)(pid: Term.Ref, stats: List[Tree])(implicit ctx: Context): PackageClause =
      kernel.PackageClause_copy(original)(pid, stats)
    def unapply(tree: Tree)(implicit ctx: Context): Option[(Term.Ref, List[Tree])] =
      kernel.isPackageClause(tree).map(x => (x.pid, x.stats))
  }

  implicit class PackageClauseAPI(self: PackageClause) {
    def pid(implicit ctx: Context): Term.Ref = kernel.PackageClause_pid(self)
    def stats(implicit ctx: Context): List[Tree] = kernel.PackageClause_stats(self)
  }

  object IsImport {
    def unapply(tree: Tree)(implicit ctx: Context): Option[Import] =
      kernel.isImport(tree)
  }

  object Import {
    def apply(impliedOnly: Boolean, expr: Term, selectors: List[ImportSelector])(implicit ctx: Context): Import =
      kernel.Import_apply(impliedOnly, expr, selectors)
    def copy(original: Import)(impliedOnly: Boolean, expr: Term, selectors: List[ImportSelector])(implicit ctx: Context): Import =
      kernel.Import_copy(original)(impliedOnly, expr, selectors)
    def unapply(tree: Tree)(implicit ctx: Context): Option[(Boolean, Term, List[ImportSelector])] =
      kernel.isImport(tree).map(x => (x.impliedOnly, x.expr, x.selectors))
  }

  implicit class ImportAPI(self: Import)  {
    def impliedOnly: Boolean = kernel.Import_impliedOnly(self)
    def expr(implicit ctx: Context): Term = kernel.Import_expr(self)
    def selectors(implicit ctx: Context): List[ImportSelector] =
      kernel.Import_selectors(self)
  }

  object IsStatement {
    /** Matches any Statement and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Statement] = kernel.isStatement(tree)
  }

  // ----- Definitions ----------------------------------------------

  object IsDefinition {
    def unapply(tree: Tree)(implicit ctx: Context): Option[Definition] = kernel.isDefinition(tree)
  }

  implicit class DefinitionAPI(self: Definition) {
    def name(implicit ctx: Context): String = kernel.Definition_name(self)
  }

  // ClassDef

  object IsClassDef {
    def unapply(tree: Tree)(implicit ctx: Context): Option[ClassDef] = kernel.isClassDef(tree)
  }

  object ClassDef {
    // TODO def apply(name: String, constr: DefDef, parents: List[TermOrTypeTree], selfOpt: Option[ValDef], body: List[Statement])(implicit ctx: Context): ClassDef
    def copy(original: ClassDef)(name: String, constr: DefDef, parents: List[TermOrTypeTree], derived: List[TypeTree], selfOpt: Option[ValDef], body: List[Statement])(implicit ctx: Context): ClassDef =
      kernel.ClassDef_copy(original)(name, constr, parents, derived, selfOpt, body)
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, DefDef, List[TermOrTypeTree], List[TypeTree], Option[ValDef], List[Statement])] =
      kernel.isClassDef(tree).map(x => (x.name, x.constructor, x.parents, x.derived, x.self, x.body))
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
    def unapply(tree: Tree)(implicit ctx: Context): Option[DefDef] = kernel.isDefDef(tree)
  }

  object DefDef {
    def apply(symbol: DefSymbol, rhsFn: List[Type] => List[List[Term]] => Option[Term])(implicit ctx: Context): DefDef =
      kernel.DefDef_apply(symbol, rhsFn)
    def copy(original: DefDef)(name: String, typeParams: List[TypeDef], paramss: List[List[ValDef]], tpt: TypeTree, rhs: Option[Term])(implicit ctx: Context): DefDef =
      kernel.DefDef_copy(original)(name, typeParams, paramss, tpt, rhs)
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, List[TypeDef], List[List[ValDef]], TypeTree, Option[Term])] =
      kernel.isDefDef(tree).map(x => (x.name, x.typeParams, x.paramss, x.returnTpt, x.rhs))
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
    def unapply(tree: Tree)(implicit ctx: Context): Option[ValDef] = kernel.isValDef(tree)
  }

  object ValDef {
    def apply(symbol: ValSymbol, rhs: Option[Term])(implicit ctx: Context): ValDef =
      kernel.ValDef_apply(symbol, rhs)
    def copy(original: ValDef)(name: String, tpt: TypeTree, rhs: Option[Term])(implicit ctx: Context): ValDef =
      kernel.ValDef_copy(original)(name, tpt, rhs)
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, TypeTree, Option[Term])] =
      kernel.isValDef(tree).map(x => (x.name, x.tpt, x.rhs))
  }

  implicit class ValDefAPI(self: ValDef) {
    def tpt(implicit ctx: Context): TypeTree = kernel.ValDef_tpt(self)
    def rhs(implicit ctx: Context): Option[Term] = kernel.ValDef_rhs(self)
    def symbol(implicit ctx: Context): ValSymbol = kernel.ValDef_symbol(self)
  }

  // TypeDef

  object IsTypeDef {
    def unapply(tree: Tree)(implicit ctx: Context): Option[TypeDef] = kernel.isTypeDef(tree)
  }

  object TypeDef {
    def apply(symbol: TypeSymbol)(implicit ctx: Context): TypeDef =
      kernel.TypeDef_apply(symbol)
    def copy(original: TypeDef)(name: String, rhs: TypeOrBoundsTree)(implicit ctx: Context): TypeDef =
      kernel.TypeDef_copy(original)(name, rhs)
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, TypeOrBoundsTree /* TypeTree | TypeBoundsTree */)] =
      kernel.isTypeDef(tree).map(x => (x.name, x.rhs))
  }

  implicit class TypeDefAPI(self: TypeDef) {
    def rhs(implicit ctx: Context): TypeOrBoundsTree = kernel.TypeDef_rhs(self)
    def symbol(implicit ctx: Context): TypeSymbol = kernel.TypeDef_symbol(self)
  }

  // PackageDef

  object IsPackageDef {
    def unapply(tree: Tree)(implicit ctx: Context): Option[PackageDef] =
      kernel.isPackageDef(tree)
  }

  implicit class PackageDefAPI(self: PackageDef) {
    def owner(implicit ctx: Context): PackageDef = kernel.PackageDef_owner(self)
    def members(implicit ctx: Context): List[Statement] = kernel.PackageDef_members(self)
    def symbol(implicit ctx: Context): PackageSymbol = kernel.PackageDef_symbol(self)
  }

  object PackageDef {
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, PackageDef)] =
      kernel.isPackageDef(tree).map(x => (x.name, x.owner))
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
      kernel.isTerm(tree)

    /** Matches any term */
    def unapply(parent: TermOrTypeTree)(implicit ctx: Context, dummy: DummyImplicit): Option[Term] =
      kernel.isTermNotTypeTree(parent)
  }

  /** Scala term. Any tree that can go in expression position. */
  object Term extends TermCoreModule {

    object IsIdent {
      /** Matches any Ident and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Ident] = kernel.isTerm_Ident(tree)
    }

    object Ref {

      /** Create a reference tree */
      def apply(sym: Symbol)(implicit ctx: Context): Ref =
        kernel.Term_Ref_apply(sym)

      // TODO def copy(original: Tree)(name: String)(implicit ctx: Context): Ref

    }

    /** Scala term identifier */
    object Ident {
      def apply(tmref: TermRef)(implicit ctx: Context): Term =
        kernel.Term_Ident_apply(tmref)

      def copy(original: Tree)(name: String)(implicit ctx: Context): Ident =
        kernel.Term_Ident_copy(original)(name)

      /** Matches a term identifier and returns its name */
      def unapply(tree: Tree)(implicit ctx: Context): Option[String] =
        kernel.isTerm_Ident(tree).map(_.name)
    }

    object IsSelect {
      /** Matches any Select and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Select] = kernel.isTerm_Select(tree)
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
        kernel.Term_Select_unique(qualifier, name)

      // TODO rename, this returns an Apply and not a Select
      /** Call an overloaded method with the given type and term parameters */
      def overloaded(qualifier: Term, name: String, targs: List[Type], args: List[Term])(implicit ctx: Context): Apply =
        kernel.Term_Select_overloaded(qualifier, name, targs, args)

      def copy(original: Tree)(qualifier: Term, name: String)(implicit ctx: Context): Select =
        kernel.Term_Select_copy(original)(qualifier, name)

      /** Matches `<qualifier: Term>.<name: String>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, String)] =
        kernel.isTerm_Select(tree).map(x => (x.qualifier, x.name))
    }

    object IsLiteral {
      /** Matches any Literal and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Literal] = kernel.isTerm_Literal(tree)
    }

    /** Scala literal constant */
    object Literal {

      /** Create a literal constant */
      def apply(constant: Constant)(implicit ctx: Context): Literal =
        kernel.Term_Literal_apply(constant)

      def copy(original: Tree)(constant: Constant)(implicit ctx: Context): Literal =
        kernel.Term_Literal_copy(original)(constant)

      /** Matches a literal constant */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Constant] =
        kernel.isTerm_Literal(tree).map(_.constant)
    }

    object IsThis {
      /** Matches any This and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[This] = kernel.isTerm_This(tree)
    }

    /** Scala `this` or `this[id]` */
    object This {

      /** Create a `this[<id: Id]>` */
      def apply(cls: ClassSymbol)(implicit ctx: Context): This =
        kernel.Term_This_apply(cls)

      def copy(original: Tree)(qual: Option[Id])(implicit ctx: Context): This =
        kernel.Term_This_copy(original)(qual)

      /** Matches `this[<id: Option[Id]>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Option[Id]] =
        kernel.isTerm_This(tree).map(_.id)

    }

    object IsNew {
      /** Matches any New and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[New] = kernel.isTerm_New(tree)
    }

    /** Scala `new` */
    object New {

      /** Create a `new <tpt: TypeTree>` */
      def apply(tpt: TypeTree)(implicit ctx: Context): New =
        kernel.Term_New_apply(tpt)

      def copy(original: Tree)(tpt: TypeTree)(implicit ctx: Context): New =
        kernel.Term_New_copy(original)(tpt)

      /** Matches a `new <tpt: TypeTree>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[TypeTree] =
        kernel.isTerm_New(tree).map(_.tpt)
    }

    object IsNamedArg {
      /** Matches any NamedArg and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[NamedArg] = kernel.isTerm_NamedArg(tree)
    }

    /** Scala named argument `x = y` in argument position */
    object NamedArg {

      /** Create a named argument `<name: String> = <value: Term>` */
      def apply(name: String, arg: Term)(implicit ctx: Context): NamedArg =
        kernel.Term_NamedArg_apply(name, arg)

      def copy(original: NamedArg)(name: String, arg: Term)(implicit ctx: Context): NamedArg =
        kernel.Term_NamedArg_copy(original)(name, arg)

      /** Matches a named argument `<name: String> = <value: Term>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(String, Term)] =
        kernel.isTerm_NamedArg(tree).map(x => (x.name, x.value))

    }

    object IsApply {
      /** Matches any Apply and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Apply] = kernel.isTerm_Apply(tree)
    }

    /** Scala parameter application */
    object Apply {

      /** Create a function application `<fun: Term>(<args: List[Term]>)` */
      def apply(fun: Term, args: List[Term])(implicit ctx: Context): Apply =
        kernel.Term_Apply_apply(fun, args)

      def copy(original: Tree)(fun: Term, args: List[Term])(implicit ctx: Context): Apply =
        kernel.Term_Apply_copy(original)(fun, args)

      /** Matches a function application `<fun: Term>(<args: List[Term]>)` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, List[Term])] =
        kernel.isTerm_Apply(tree).map(x => (x.fun, x.args))
    }

    object IsTypeApply {
      /** Matches any TypeApply and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[TypeApply] =
        kernel.isTerm_TypeApply(tree)
    }

    /** Scala type parameter application */
    object TypeApply {

      /** Create a function type application `<fun: Term>[<args: List[TypeTree]>]` */
      def apply(fun: Term, args: List[TypeTree])(implicit ctx: Context): TypeApply =
        kernel.Term_TypeApply_apply(fun, args)

      def copy(original: Tree)(fun: Term, args: List[TypeTree])(implicit ctx: Context): TypeApply =
        kernel.Term_TypeApply_copy(original)(fun, args)

      /** Matches a function type application `<fun: Term>[<args: List[TypeTree]>]` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, List[TypeTree])] =
        kernel.isTerm_TypeApply(tree).map(x => (x.fun, x.args))

    }

    object IsSuper {
      /** Matches any Super and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Super] = kernel.isTerm_Super(tree)
    }

    /** Scala `x.super` or `x.super[id]` */
    object Super {

      /** Creates a `<qualifier: Term>.super[<id: Option[Id]>` */
      def apply(qual: Term, mix: Option[Id])(implicit ctx: Context): Super =
        kernel.Term_Super_apply(qual, mix)

      def copy(original: Tree)(qual: Term, mix: Option[Id])(implicit ctx: Context): Super =
        kernel.Term_Super_copy(original)(qual, mix)

      /** Matches a `<qualifier: Term>.super[<id: Option[Id]>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Option[Id])] =
        kernel.isTerm_Super(tree).map(x => (x.qualifier, x.id))
    }

    object IsTyped {
      /** Matches any Typed and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Typed] = kernel.isTerm_Typed(tree)
    }

    /** Scala ascription `x: T` */
    object Typed {

      /** Create a type ascription `<x: Term>: <tpt: TypeTree>` */
      def apply(expr: Term, tpt: TypeTree)(implicit ctx: Context): Typed =
        kernel.Term_Typed_apply(expr, tpt)

      def copy(original: Tree)(expr: Term, tpt: TypeTree)(implicit ctx: Context): Typed =
        kernel.Term_Typed_copy(original)(expr, tpt)

      /** Matches `<expr: Term>: <tpt: TypeTree>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, TypeTree)] =
        kernel.isTerm_Typed(tree).map(x => (x.expr, x.tpt))

    }

    object IsAssign {
      /** Matches any Assign and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Assign] = kernel.isTerm_Assign(tree)
    }

    /** Scala assign `x = y` */
    object Assign {

      /** Create an assignment `<lhs: Term> = <rhs: Term>` */
      def apply(lhs: Term, rhs: Term)(implicit ctx: Context): Assign =
        kernel.Term_Assign_apply(lhs, rhs)

      def copy(original: Tree)(lhs: Term, rhs: Term)(implicit ctx: Context): Assign =
        kernel.Term_Assign_copy(original)(lhs, rhs)

      /** Matches an assignment `<lhs: Term> = <rhs: Term>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Term)] =
        kernel.isTerm_Assign(tree).map(x => (x.lhs, x.rhs))
    }

    object IsBlock {
      /** Matches any Block and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Block] = kernel.isTerm_Block(tree)
    }

    /** Scala code block `{ stat0; ...; statN; expr }` term */
    object Block {

      /** Creates a block `{ <statements: List[Statement]>; <expr: Term> }` */
      def apply(stats: List[Statement], expr: Term)(implicit ctx: Context): Block =
        kernel.Term_Block_apply(stats, expr)

      def copy(original: Tree)(stats: List[Statement], expr: Term)(implicit ctx: Context): Block =
        kernel.Term_Block_copy(original)(stats, expr)

      /** Matches a block `{ <statements: List[Statement]>; <expr: Term> }` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(List[Statement], Term)] =
        kernel.isTerm_Block(tree).map(x => (x.statements, x.expr))
    }

    object IsLambda {
      /** Matches any Lambda and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Lambda] = kernel.isTerm_Lambda(tree)
    }

    object Lambda {

      def apply(meth: Term, tpt: Option[TypeTree])(implicit ctx: Context): Lambda =
        kernel.Term_Lambda_apply(meth, tpt)

      def copy(original: Tree)(meth: Tree, tpt: Option[TypeTree])(implicit ctx: Context): Lambda =
        kernel.Term_Lambda_copy(original)(meth, tpt)

      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Option[TypeTree])] =
        kernel.isTerm_Lambda(tree).map(x => (x.meth, x.tptOpt))
    }

    object IsIf {
      /** Matches any If and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[If] = kernel.isTerm_If(tree)
    }

    /** Scala `if`/`else` term */
    object If {

      /** Create an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>` */
      def apply(cond: Term, thenp: Term, elsep: Term)(implicit ctx: Context): If =
        kernel.Term_If_apply(cond, thenp, elsep)

      def copy(original: Tree)(cond: Term, thenp: Term, elsep: Term)(implicit ctx: Context): If =
        kernel.Term_If_copy(original)(cond, thenp, elsep)

      /** Matches an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Term, Term)] =
        kernel.isTerm_If(tree).map(x => (x.cond, x.thenp, x.elsep))

    }

    object IsMatch {
      /** Matches any Match and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Match] = kernel.isTerm_Match(tree)
    }

    /** Scala `match` term */
    object Match {

      /** Creates a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }` */
      def apply(selector: Term, cases: List[CaseDef])(implicit ctx: Context): Match =
        kernel.Term_Match_apply(selector, cases)

      def copy(original: Tree)(selector: Term, cases: List[CaseDef])(implicit ctx: Context): Match =
        kernel.Term_Match_copy(original)(selector, cases)

      /** Matches a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, List[CaseDef])] =
        kernel.isTerm_Match(tree).map(x => (x.scrutinee, x.cases))

    }

    object IsTry {
      /** Matches any Try and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Try] = kernel.isTerm_Try(tree)
    }

    /** Scala `try`/`catch`/`finally` term */
    object Try {

      /** Create a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>` */
      def apply(expr: Term, cases: List[CaseDef], finalizer: Option[Term])(implicit ctx: Context): Try =
        kernel.Term_Try_apply(expr, cases, finalizer)

      def copy(original: Tree)(expr: Term, cases: List[CaseDef], finalizer: Option[Term])(implicit ctx: Context): Try =
        kernel.Term_Try_copy(original)(expr, cases, finalizer)

      /** Matches a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, List[CaseDef], Option[Term])] =
        kernel.isTerm_Try(tree).map(x => (x.body, x.cases, x.finalizer))

    }

    object IsReturn {
      /** Matches any Return and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Return] = kernel.isTerm_Return(tree)
    }

    /** Scala local `return` */
    object Return {

      /** Creates `return <expr: Term>` */
      def apply(expr: Term)(implicit ctx: Context): Return =
        kernel.Term_Return_apply(expr)

      def copy(original: Tree)(expr: Term)(implicit ctx: Context): Return =
        kernel.Term_Return_copy(original)(expr)

      /** Matches `return <expr: Term>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Term] =
        kernel.isTerm_Return(tree).map(_.expr)

    }

    object IsRepeated {
      /** Matches any Repeated and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Repeated] = kernel.isTerm_Repeated(tree)
    }

    object Repeated {

      def apply(elems: List[Term], tpt: TypeTree)(implicit ctx: Context): Repeated =
        kernel.Term_Repeated_apply(elems, tpt)

      def copy(original: Tree)(elems: List[Term], tpt: TypeTree)(implicit ctx: Context): Repeated =
        kernel.Term_Repeated_copy(original)(elems, tpt)

      def unapply(tree: Tree)(implicit ctx: Context): Option[(List[Term], TypeTree)] =
        kernel.isTerm_Repeated(tree).map(x => (x.elems, x.elemtpt))

    }

    object IsInlined {
      /** Matches any Inlined and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Inlined] = kernel.isTerm_Inlined(tree)
    }

    object Inlined {

      def apply(call: Option[TermOrTypeTree], bindings: List[Definition], expansion: Term)(implicit ctx: Context): Inlined =
        kernel.Term_Inlined_apply(call, bindings, expansion)

      def copy(original: Tree)(call: Option[TermOrTypeTree], bindings: List[Definition], expansion: Term)(implicit ctx: Context): Inlined =
        kernel.Term_Inlined_copy(original)(call, bindings, expansion)

      def unapply(tree: Tree)(implicit ctx: Context): Option[(Option[TermOrTypeTree], List[Definition], Term)] =
        kernel.isTerm_Inlined(tree).map(x => (x.call, x.bindings, x.body))

    }

    object IsSelectOuter {
      /** Matches any SelectOuter and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[SelectOuter] = kernel.isTerm_SelectOuter(tree)
    }

    object SelectOuter {

      def apply(qualifier: Term, name: String, levels: Int)(implicit ctx: Context): SelectOuter =
        kernel.Term_SelectOuter_apply(qualifier, name, levels)

      def copy(original: Tree)(qualifier: Term, name: String, levels: Int)(implicit ctx: Context): SelectOuter =
        kernel.Term_SelectOuter_copy(original)(qualifier, name, levels)

      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Int, Type)] = // TODO homogenize order of parameters
        kernel.isTerm_SelectOuter(tree).map(x => (x.qualifier, x.level, x.tpe))

    }

    object IsWhile {
      /** Matches any While and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[While] = kernel.isTerm_While(tree)
    }

    object While {

      /** Creates a while loop `while (<cond>) <body>` and returns (<cond>, <body>) */
      def apply(cond: Term, body: Term)(implicit ctx: Context): While =
        kernel.Term_While_apply(cond, body)

      def copy(original: Tree)(cond: Term, body: Term)(implicit ctx: Context): While =
        kernel.Term_While_copy(original)(cond, body)

      /** Extractor for while loops. Matches `while (<cond>) <body>` and returns (<cond>, <body>) */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Term)] =
        kernel.isTerm_While(tree).map(x => (x.cond, x.body))

    }
  }

  implicit class Term_IdentAPI(self: Term.Ident) {
    def name(implicit ctx: Context): String = kernel.Term_Ident_name(self)
  }

  implicit class Term_SelectAPI(self: Term.Select) {
    def qualifier(implicit ctx: Context): Term = kernel.Term_Select_qualifier(self)
    def name(implicit ctx: Context): String = kernel.Term_Select_name(self)
    def signature(implicit ctx: Context): Option[Signature] = kernel.Term_Select_signature(self)
  }

  implicit class Term_LiteralAPI(self: Term.Literal) {
    def constant(implicit ctx: Context): Constant = kernel.Term_Literal_constant(self)
  }

  implicit class Term_ThisAPI(self: Term.This) {
    def id(implicit ctx: Context): Option[Id] = kernel.Term_This_id(self)
  }

  implicit class Term_NewAPI(self: Term.New) {
    def tpt(implicit ctx: Context): TypeTree = kernel.Term_New_tpt(self)
  }

  implicit class Term_NamedArgAPI(self: Term.NamedArg) {
    def name(implicit ctx: Context): String = kernel.Term_NamedArg_name(self)
    def value(implicit ctx: Context): Term = kernel.Term_NamedArg_value(self)
  }

  implicit class Term_ApplyAPI(self: Term.Apply) {
    def fun(implicit ctx: Context): Term = kernel.Term_Apply_fun(self)
    def args(implicit ctx: Context): List[Term] = kernel.Term_Apply_args(self)
  }

  implicit class Term_TypeApplyAPI(self: Term.TypeApply) {
    def fun(implicit ctx: Context): Term = kernel.Term_TypeApply_fun(self)
    def args(implicit ctx: Context): List[TypeTree] = kernel.Term_TypeApply_args(self)
  }

  implicit class Term_SuperAPI(self: Term.Super) {
    def qualifier(implicit ctx: Context): Term = kernel.Term_Super_qualifier(self)
    def id(implicit ctx: Context): Option[Id] = kernel.Term_Super_id(self)
  }

  implicit class Term_TypedAPI(self: Term.Typed) {
    def expr(implicit ctx: Context): Term = kernel.Term_Typed_expr(self)
    def tpt(implicit ctx: Context): TypeTree = kernel.Term_Typed_tpt(self)
  }

  implicit class Term_AssignAPI(self: Term.Assign) {
    def lhs(implicit ctx: Context): Term = kernel.Term_Assign_lhs(self)
    def rhs(implicit ctx: Context): Term = kernel.Term_Assign_rhs(self)
  }

  implicit class Term_BlockAPI(self: Term.Block) {
    def statements(implicit ctx: Context): List[Statement] = kernel.Term_Block_statements(self)
    def expr(implicit ctx: Context): Term = kernel.Term_Block_expr(self)
  }

  implicit class Term_LambdaAPI(self: Term.Lambda) {
    def meth(implicit ctx: Context): Term = kernel.Term_Lambda_meth(self)
    def tptOpt(implicit ctx: Context): Option[TypeTree] = kernel.Term_Lambda_tptOpt(self)
  }

  implicit class Term_IfAPI(self: Term.If) {
    def cond(implicit ctx: Context): Term = kernel.Term_If_cond(self)
    def thenp(implicit ctx: Context): Term = kernel.Term_If_thenp(self)
    def elsep(implicit ctx: Context): Term = kernel.Term_If_elsep(self)
  }

  implicit class Term_MatchAPI(self: Term.Match) {
    def scrutinee(implicit ctx: Context): Term = kernel.Term_Match_scrutinee(self)
    def cases(implicit ctx: Context): List[CaseDef] = kernel.Term_Match_cases(self)
  }

  implicit class Term_TryAPI(self: Term.Try) {
    def body(implicit ctx: Context): Term = kernel.Term_Try_body(self)
    def cases(implicit ctx: Context): List[CaseDef] = kernel.Term_Try_cases(self)
    def finalizer(implicit ctx: Context): Option[Term] = kernel.Term_Try_finalizer(self)
  }

  implicit class Term_ReturnAPI(self: Term.Return) {
    def expr(implicit ctx: Context): Term = kernel.Term_Return_expr(self)
  }

  implicit class Term_RepeatedAPI(self: Term.Repeated) {
    def elems(implicit ctx: Context): List[Term] = kernel.Term_Repeated_elems(self)
    def elemtpt(implicit ctx: Context): TypeTree = kernel.Term_Repeated_elemtpt(self)
  }

  implicit class Term_InlinedAPI(self: Term.Inlined) {
    def call(implicit ctx: Context): Option[TermOrTypeTree] = kernel.Term_Inlined_call(self)
    def bindings(implicit ctx: Context): List[Definition] = kernel.Term_Inlined_bindings(self)
    def body(implicit ctx: Context): Term = kernel.Term_Inlined_body(self)
  }

  implicit class Term_SelectOuterAPI(self: Term.SelectOuter) {
    def qualifier(implicit ctx: Context): Term = kernel.Term_SelectOuter_qualifier(self)
    def level(implicit ctx: Context): Int = kernel.Term_SelectOuter_level(self)
    def tpe(implicit ctx: Context): Type = kernel.Term_SelectOuter_tpe(self)
  }

  implicit class Term_WhileAPI(self: Term.While) {
    def cond(implicit ctx: Context): Term = kernel.Term_While_cond(self)
    def body(implicit ctx: Context): Term = kernel.Term_While_body(self)
  }

}
