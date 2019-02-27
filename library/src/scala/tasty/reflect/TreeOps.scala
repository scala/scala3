package scala.tasty
package reflect

trait TreeOps extends Core {

  // Decorators

  implicit def termAsTermOrTypeTree(term: Term): TermOrTypeTree

  // ----- Tree -----------------------------------------------------

  implicit class TreeAPI(self: Tree) {
    /** Position in the source code */
    def pos(implicit ctx: Context): Position = kernel.Tree_pos(self)

    def symbol(implicit ctx: Context): Symbol = kernel.Tree_symbol(self)
  }

  val IsPackageClause: IsPackageClauseModule
  abstract class IsPackageClauseModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[PackageClause]
  }

  val PackageClause: PackageClauseModule
  abstract class PackageClauseModule {
    def apply(pid: Term.Ref, stats: List[Tree])(implicit ctx: Context): PackageClause
    def copy(original: PackageClause)(pid: Term.Ref, stats: List[Tree])(implicit ctx: Context): PackageClause
    def unapply(tree: Tree)(implicit ctx: Context): Option[(Term.Ref, List[Tree])]
  }

  implicit class PackageClauseAPI(self: PackageClause) {
    def pid(implicit ctx: Context): Term.Ref = kernel.PackageClause_pid(self)
    def stats(implicit ctx: Context): List[Tree] = kernel.PackageClause_stats(self)
  }

  val IsImport: IsImportModule
  abstract class IsImportModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[Import]
  }

  val Import: ImportModule
  abstract class ImportModule {
    def apply(impliedOnly: Boolean, expr: Term, selectors: List[ImportSelector])(implicit ctx: Context): Import
    def copy(original: Import)(impliedOnly: Boolean, expr: Term, selectors: List[ImportSelector])(implicit ctx: Context): Import
    def unapply(imp: Tree)(implicit ctx: Context): Option[(Boolean, Term, List[ImportSelector])]
  }

  implicit class ImportAPI(self: Import)  {
    def impliedOnly: Boolean = kernel.Import_impliedOnly(self)
    def expr(implicit ctx: Context): Term = kernel.Import_expr(self)
    def selectors(implicit ctx: Context): List[ImportSelector] = kernel.Import_selectors(self)
  }

  val IsStatement: IsStatementModule
  abstract class IsStatementModule {
    /** Matches any Statement and returns it */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Statement]
  }

  // ----- Definitions ----------------------------------------------

  val IsDefinition: IsDefinitionModule
  abstract class IsDefinitionModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[Definition]
  }

  implicit class DefinitionAPI(self: Definition) {
    def name(implicit ctx: Context): String = kernel.Definition_name(self)
  }

  // ClassDef

  val IsClassDef: IsClassDefModule
  abstract class IsClassDefModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[ClassDef]
  }

  val ClassDef: ClassDefModule
  abstract class ClassDefModule {
    // TODO def apply(name: String, constr: DefDef, parents: List[TermOrTypeTree], selfOpt: Option[ValDef], body: List[Statement])(implicit ctx: Context): ClassDef
    def copy(original: ClassDef)(name: String, constr: DefDef, parents: List[TermOrTypeTree], derived: List[TypeTree], selfOpt: Option[ValDef], body: List[Statement])(implicit ctx: Context): ClassDef
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, DefDef, List[TermOrTypeTree], List[TypeTree], Option[ValDef], List[Statement])]
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

  val IsDefDef: IsDefDefModule
  abstract class IsDefDefModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[DefDef]
  }

  val DefDef: DefDefModule
  abstract class DefDefModule {
    def apply(symbol: DefSymbol, rhsFn: List[Type] => List[List[Term]] => Option[Term])(implicit ctx: Context): DefDef
    def copy(original: DefDef)(name: String, typeParams: List[TypeDef], paramss: List[List[ValDef]], tpt: TypeTree, rhs: Option[Term])(implicit ctx: Context): DefDef
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, List[TypeDef],  List[List[ValDef]], TypeTree, Option[Term])]
  }

  implicit class DefDefAPI(self: DefDef) {
    def typeParams(implicit ctx: Context): List[TypeDef] = kernel.DefDef_typeParams(self)
    def paramss(implicit ctx: Context): List[List[ValDef]] = kernel.DefDef_paramss(self)
    def returnTpt(implicit ctx: Context): TypeTree = kernel.DefDef_returnTpt(self)
    def rhs(implicit ctx: Context): Option[Term] = kernel.DefDef_rhs(self)
    def symbol(implicit ctx: Context): DefSymbol = kernel.DefDef_symbol(self)
  }

  // ValDef

  val IsValDef: IsValDefModule
  abstract class IsValDefModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[ValDef]
  }

  val ValDef: ValDefModule
  abstract class ValDefModule {
    def apply(sym: ValSymbol, rhs: Option[Term])(implicit ctx: Context): ValDef
    def copy(original: ValDef)(name: String, tpt: TypeTree, rhs: Option[Term])(implicit ctx: Context): ValDef
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, TypeTree, Option[Term])]
  }

  implicit class ValDefAPI(self: ValDef) {
    def tpt(implicit ctx: Context): TypeTree = kernel.ValDef_tpt(self)
    def rhs(implicit ctx: Context): Option[Term] = kernel.ValDef_rhs(self)
    def symbol(implicit ctx: Context): ValSymbol = kernel.ValDef_symbol(self)
  }

  // TypeDef

  val IsTypeDef: IsTypeDefModule
  abstract class IsTypeDefModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[TypeDef]
  }

  val TypeDef: TypeDefModule
  abstract class TypeDefModule {
    def apply(symbol: TypeSymbol)(implicit ctx: Context): TypeDef
    def copy(original: TypeDef)(name: String, rhs: TypeOrBoundsTree)(implicit ctx: Context): TypeDef
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, TypeOrBoundsTree /* TypeTree | TypeBoundsTree */)]
  }

  implicit class TypeDefAPI(self: TypeDef) {
    def rhs(implicit ctx: Context): TypeOrBoundsTree = kernel.TypeDef_rhs(self)
    def symbol(implicit ctx: Context): TypeSymbol = kernel.TypeDef_symbol(self)
  }

  // PackageDef

  val IsPackageDef: IsPackageDefModule
  abstract class IsPackageDefModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[PackageDef]
  }

  implicit class PackageDefAPI(self: PackageDef) {
    def owner(implicit ctx: Context): PackageDef = kernel.PackageDef_owner(self)
    def members(implicit ctx: Context): List[Statement] = kernel.PackageDef_members(self)
    def symbol(implicit ctx: Context): PackageSymbol = kernel.PackageDef_symbol(self)
  }

  val PackageDef: PackageDefModule
  abstract class PackageDefModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, PackageDef)]
  }

  // ----- Terms ----------------------------------------------------

  implicit class TermAPI(self: Term) {
    def tpe(implicit ctx: Context): Type = kernel.Term_tpe(self)
    def pos(implicit ctx: Context): Position = kernel.Term_pos(self)
    def underlyingArgument(implicit ctx: Context): Term = kernel.Term_underlyingArgument(self)
    def underlying(implicit ctx: Context): Term = kernel.Term_underlying(self)
  }

  val IsTerm: IsTermModule
  abstract class IsTermModule {
    /** Matches any term */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Term]
    /** Matches any term */
    def unapply(parent: TermOrTypeTree)(implicit ctx: Context, dummy: DummyImplicit): Option[Term]
  }

  /** Scala term. Any tree that can go in expression position. */
  val Term: TermModule
  abstract class TermModule extends TermCoreModule {

    val IsIdent: IsIdentModule
    abstract class IsIdentModule {
      /** Matches any Ident and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Ident]
    }

    val Ref: RefModule
    abstract class RefModule {

      /** Create a reference tree */
      def apply(sym: Symbol)(implicit ctx: Context): Ref

      // TODO def copy(original: Tree)(name: String)(implicit ctx: Context): Ref

    }

    /** Scala term identifier */
    val Ident: IdentModule
    abstract class IdentModule {
      def apply(tmref: TermRef)(implicit ctx: Context): Term

      def copy(original: Tree)(name: String)(implicit ctx: Context): Ident

      /** Matches a term identifier and returns its name */
      def unapply(tree: Tree)(implicit ctx: Context): Option[String]

    }

    val IsSelect: IsSelectModule
    abstract class IsSelectModule {
      /** Matches any Select and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Select]
    }

    /** Scala term selection */
    val Select: SelectModule
    abstract class SelectModule {
      /** Select a field or a non-overloaded method by name
       *
       *  @note The method will produce an assertion error if the selected
       *        method is overloaded. The method `overloaded` should be used
       *        in that case.
       */
      def unique(qualifier: Term, name: String)(implicit ctx: Context): Select

      /** Call an overloaded method with the given type and term parameters */
      def overloaded(qualifier: Term, name: String, targs: List[Type], args: List[Term])(implicit ctx: Context): Apply

      def copy(original: Tree)(qualifier: Term, name: String)(implicit ctx: Context): Select

      /** Matches `<qual: Term>.<name: String>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, String)]
    }

    val IsLiteral: IsLiteralModule
    abstract class IsLiteralModule {
      /** Matches any Literal and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Literal]
    }

    /** Scala literal constant */
    val Literal: LiteralModule
    abstract class LiteralModule {

      /** Create a literal constant */
      def apply(constant: Constant)(implicit ctx: Context): Literal

      def copy(original: Tree)(constant: Constant)(implicit ctx: Context): Literal

      /** Matches a literal constant */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Constant]

    }

    val IsThis: IsThisModule
    abstract class IsThisModule {
      /** Matches any This and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[This]
    }

    /** Scala `this` or `this[id]` */
    val This: ThisModule
    abstract class ThisModule {

      /** Create a `this[<id: Id]>` */
      def apply(cls: ClassSymbol)(implicit ctx: Context): This

      def copy(original: Tree)(qual: Option[Id])(implicit ctx: Context): This

      /** Matches `this[<id: Option[Id]>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Option[Id]]

    }

    val IsNew: IsNewModule
    abstract class IsNewModule {
      /** Matches any New and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[New]
    }

    /** Scala `new` */
    val New: NewModule
    abstract class NewModule {

      /** Create a `new <tpt: TypeTree>` */
      def apply(tpt: TypeTree)(implicit ctx: Context): New

      def copy(original: Tree)(tpt: TypeTree)(implicit ctx: Context): New

      /** Matches a `new <tpt: TypeTree>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[TypeTree]

    }

    val IsNamedArg: IsNamedArgModule
    abstract class IsNamedArgModule {
      /** Matches any NamedArg and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[NamedArg]
    }

    /** Scala named argument `x = y` in argument position */
    val NamedArg: NamedArgModule
    abstract class NamedArgModule {

      /** Create a named argument `<name: String> = <value: Term>` */
      def apply(name: String, arg: Term)(implicit ctx: Context): NamedArg

      def copy(tree: NamedArg)(name: String, arg: Term)(implicit ctx: Context): NamedArg

      /** Matches a named argument `<name: String> = <value: Term>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(String, Term)]

    }

    val IsApply: IsApplyModule
    abstract class IsApplyModule {
      /** Matches any Apply and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Apply]
    }

    /** Scala parameter application */
    val Apply: ApplyModule
    abstract class ApplyModule {

      /** Create a function application `<fun: Term>(<args: List[Term]>)` */
      def apply(fn: Term, args: List[Term])(implicit ctx: Context): Apply

      def copy(original: Tree)(fun: Term, args: List[Term])(implicit ctx: Context): Apply

      /** Matches a function application `<fun: Term>(<args: List[Term]>)` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, List[Term])]

    }

    val IsTypeApply: IsTypeApplyModule
    abstract class IsTypeApplyModule {
      /** Matches any TypeApply and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[TypeApply]
    }

    /** Scala type parameter application */
    val TypeApply: TypeApplyModule
    abstract class TypeApplyModule {

      /** Create a function type application `<fun: Term>[<args: List[TypeTree]>]` */
      def apply(fn: Term, args: List[TypeTree])(implicit ctx: Context): TypeApply

      def copy(original: Tree)(fun: Term, args: List[TypeTree])(implicit ctx: Context): TypeApply

      /** Matches a function type application `<fun: Term>[<args: List[TypeTree]>]` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, List[TypeTree])]

    }

    val IsSuper: IsSuperModule
    abstract class IsSuperModule {
      /** Matches any Super and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Super]
    }

    /** Scala `x.super` or `x.super[id]` */
    val Super: SuperModule
    abstract class SuperModule {

      /** Creates a `<qualifier: Term>.super[<id: Option[Id]>` */
      def apply(qual: Term, mix: Option[Id])(implicit ctx: Context): Super

      def copy(original: Tree)(qual: Term, mix: Option[Id])(implicit ctx: Context): Super

      /** Matches a `<qualifier: Term>.super[<id: Option[Id]>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Option[Id])]

    }

    val IsTyped: IsTypedModule
    abstract class IsTypedModule {
      /** Matches any Typed and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Typed]
    }

    /** Scala ascription `x: T` */
    val Typed: TypedModule
    abstract class TypedModule {

      /** Create a type ascription `<x: Term>: <tpt: TypeTree>` */
      def apply(expr: Term, tpt: TypeTree)(implicit ctx: Context): Typed

      def copy(original: Tree)(expr: Term, tpt: TypeTree)(implicit ctx: Context): Typed

      /** Matches `<expr: Term>: <tpt: TypeTree>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, TypeTree)]

    }

    val IsAssign: IsAssignModule
    abstract class IsAssignModule {
      /** Matches any Assign and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Assign]
    }

    /** Scala assign `x = y` */
    val Assign: AssignModule
    abstract class AssignModule {

      /** Create an assignment `<lhs: Term> = <rhs: Term>` */
      def apply(lhs: Term, rhs: Term)(implicit ctx: Context): Assign

      def copy(original: Tree)(lhs: Term, rhs: Term)(implicit ctx: Context): Assign

      /** Matches an assignment `<lhs: Term> = <rhs: Term>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Term)]

    }

    val IsBlock: IsBlockModule
    abstract class IsBlockModule {
      /** Matches any Block and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Block]
    }

    /** Scala code block `{ stat0; ...; statN; expr }` term */
    val Block: BlockModule
    abstract class BlockModule {

      /** Creates a block `{ <statements: List[Statement]>; <expr: Term> }` */
      def apply(stats: List[Statement], expr: Term)(implicit ctx: Context): Block

      def copy(original: Tree)(stats: List[Statement], expr: Term)(implicit ctx: Context): Block

      /** Matches a block `{ <statements: List[Statement]>; <expr: Term> }` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(List[Statement], Term)]

    }

    val IsLambda: IsLambdaModule
    abstract class IsLambdaModule {
      /** Matches any Lambda and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Lambda]
    }

    val Lambda: LambdaModule
    abstract class LambdaModule {

      def apply(meth: Term, tpt: Option[TypeTree])(implicit ctx: Context): Lambda

      def copy(original: Tree)(meth: Tree, tpt: Option[TypeTree])(implicit ctx: Context): Lambda

      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Option[TypeTree])]

    }

    val IsIf: IsIfModule
    abstract class IsIfModule {
      /** Matches any If and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[If]
    }

    /** Scala `if`/`else` term */
    val If: IfModule
    abstract class IfModule {

      /** Create an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>` */
      def apply(cond: Term, thenp: Term, elsep: Term)(implicit ctx: Context): If

      def copy(original: Tree)(cond: Term, thenp: Term, elsep: Term)(implicit ctx: Context): If

      /** Matches an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Term, Term)]

    }

    val IsMatch: IsMatchModule
    abstract class IsMatchModule {
      /** Matches any Match and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Match]
    }

    /** Scala `match` term */
    val Match: MatchModule
    abstract class MatchModule {

      /** Creates a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }` */
      def apply(selector: Term, cases: List[CaseDef])(implicit ctx: Context): Match

      def copy(original: Tree)(selector: Term, cases: List[CaseDef])(implicit ctx: Context): Match

      /** Matches a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, List[CaseDef])]

    }

    val IsTry: IsTryModule
    abstract class IsTryModule {
      /** Matches any Try and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Try]
    }

    /** Scala `try`/`catch`/`finally` term */
    val Try: TryModule
    abstract class TryModule {

      /** Create a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>` */
      def apply(expr: Term, cases: List[CaseDef], finalizer: Option[Term])(implicit ctx: Context): Try

      def copy(original: Tree)(expr: Term, cases: List[CaseDef], finalizer: Option[Term])(implicit ctx: Context): Try

      /** Matches a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, List[CaseDef], Option[Term])]

    }

    val IsReturn: IsReturnModule
    abstract class IsReturnModule {
      /** Matches any Return and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Return]
    }

    /** Scala local `return` */
    val Return: ReturnModule
    abstract class ReturnModule {

      /** Creates `return <expr: Term>` */
      def apply(expr: Term)(implicit ctx: Context): Return

      def copy(original: Tree)(expr: Term)(implicit ctx: Context): Return

      /** Matches `return <expr: Term>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Term]

    }

    val IsRepeated: IsRepeatedModule
    abstract class IsRepeatedModule {
      /** Matches any Repeated and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Repeated]
    }

    val Repeated: RepeatedModule
    abstract class RepeatedModule {

      def apply(elems: List[Term], tpt: TypeTree)(implicit ctx: Context): Repeated

      def copy(original: Tree)(elems: List[Term], tpt: TypeTree)(implicit ctx: Context): Repeated

      def unapply(tree: Tree)(implicit ctx: Context): Option[(List[Term], TypeTree)]

    }

    val IsInlined: IsInlinedModule
    abstract class IsInlinedModule {
      /** Matches any Inlined and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Inlined]
    }

    val Inlined: InlinedModule
    abstract class InlinedModule {

      def apply(call: Option[TermOrTypeTree], bindings: List[Definition], expansion: Term)(implicit ctx: Context): Inlined

      def copy(original: Tree)(call: Option[TermOrTypeTree], bindings: List[Definition], expansion: Term)(implicit ctx: Context): Inlined

      def unapply(tree: Tree)(implicit ctx: Context): Option[(Option[TermOrTypeTree], List[Definition], Term)]

    }

    val IsSelectOuter: IsSelectOuterModule
    abstract class IsSelectOuterModule {
      /** Matches any SelectOuter and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[SelectOuter]
    }

    val SelectOuter: SelectOuterModule
    abstract class SelectOuterModule {

      def apply(qualifier: Term, name: String, levels: Int)(implicit ctx: Context): SelectOuter

      def copy(original: Tree)(qualifier: Term, name: String, levels: Int)(implicit ctx: Context): SelectOuter

      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Int, Type)]

    }

    val IsWhile: IsWhileModule
    abstract class IsWhileModule {
      /** Matches any While and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[While]
    }

    val While: WhileModule
    abstract class WhileModule {

      /** Creates a while loop `while (<cond>) <body>` and returns (<cond>, <body>) */
      def apply(cond: Term, body: Term)(implicit ctx: Context): While

      def copy(original: Tree)(cond: Term, body: Term)(implicit ctx: Context): While

      /** Extractor for while loops. Matches `while (<cond>) <body>` and returns (<cond>, <body>) */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Term)]

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
