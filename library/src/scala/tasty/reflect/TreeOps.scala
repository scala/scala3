package scala.tasty
package reflect

trait TreeOps extends Core {

  trait TreeAPI {
    /** Position in the source code */
    def pos(implicit ctx: Context): Position

    def symbol(implicit ctx: Context): Symbol
  }
  implicit def TreeDeco(tree: Tree): TreeAPI

  val IsPackageClause: IsPackageClauseModule
  abstract class IsPackageClauseModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[PackageClause]
  }

  val PackageClause: PackageClauseExtractor
  abstract class PackageClauseExtractor {
    def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, List[Tree])]
  }

  trait PackageClauseAPI {

  }
  implicit def PackageClauseDeco(pack: PackageClause): PackageClauseAPI

  // ----- Statements -----------------------------------------------

  val Import: ImportExtractor
  abstract class ImportExtractor {
    def unapply(imp: Tree)(implicit ctx: Context): Option[(Term, List[ImportSelector])]
  }

  trait ImportAPI {
    def expr(implicit ctx: Context): Term
    def selector(implicit ctx: Context): List[ImportSelector]
  }
  implicit def ImportDeco(imp: Import): ImportAPI

  // ----- Definitions ----------------------------------------------

  val IsDefinition: IsDefinitionModule
  abstract class IsDefinitionModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[Definition]
  }

  trait DefinitionAPI {
    def name(implicit ctx: Context): String
  }
  implicit def DefinitionDeco(definition: Definition): DefinitionAPI

  // ClassDef

  val IsClassDef: IsClassDefModule
  abstract class IsClassDefModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[ClassDef]
  }

  val ClassDef: ClassDefExtractor
  abstract class ClassDefExtractor {
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, DefDef, List[TermOrTypeTree], Option[ValDef], List[Statement])]
  }

  trait ClassDefAPI {
    def constructor(implicit ctx: Context): DefDef
    def parents(implicit ctx: Context): List[TermOrTypeTree]
    def self(implicit ctx: Context): Option[ValDef]
    def body(implicit ctx: Context): List[Statement]

    def symbol(implicit ctx: Context): ClassSymbol
  }
  implicit def ClassDefDeco(cdef: ClassDef): ClassDefAPI

  // DefDef

  val IsDefDef: IsDefDefModule
  abstract class IsDefDefModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[DefDef]
  }

  val DefDef: DefDefExtractor
  abstract class DefDefExtractor {
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, List[TypeDef],  List[List[ValDef]], TypeTree, Option[Term])]
  }

  trait DefDefAPI {
    def typeParams(implicit ctx: Context): List[TypeDef]
    def paramss(implicit ctx: Context): List[List[ValDef]]
    def returnTpt(implicit ctx: Context): TypeTree
    def rhs(implicit ctx: Context): Option[Term]

    def symbol(implicit ctx: Context): DefSymbol
  }
  implicit def DefDefDeco(ddef: DefDef): DefDefAPI

  // ValDef

  val IsValDef: IsValDefModule
  abstract class IsValDefModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[ValDef]
  }

  val ValDef: ValDefExtractor
  abstract class ValDefExtractor {
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, TypeTree, Option[Term])]
  }

  trait ValDefAPI {
    def tpt(implicit ctx: Context): TypeTree
    def rhs(implicit ctx: Context): Option[Term]

    def symbol(implicit ctx: Context): ValSymbol
  }
  implicit def ValDefDeco(vdef: ValDef): ValDefAPI

  // TypeDef

  val IsTypeDef: IsTypeDefModule
  abstract class IsTypeDefModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[TypeDef]
  }

  val TypeDef: TypeDefExtractor
  abstract class TypeDefExtractor {
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, TypeOrBoundsTree /* TypeTree | TypeBoundsTree */)]
  }

  trait TypeDefAPI {
    def rhs(implicit ctx: Context): TypeOrBoundsTree
    def symbol(implicit ctx: Context): TypeSymbol
  }
  implicit def TypeDefDeco(tdef: TypeDef): TypeDefAPI

  // PackageDef

  val IsPackageDef: IsPackageDefModule
  abstract class IsPackageDefModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[PackageDef]
  }

  trait PackageDefAPI {
    def owner(implicit ctx: Context): PackageDef
    def members(implicit ctx: Context): List[Statement]
    def symbol(implicit ctx: Context): PackageSymbol
  }
  implicit def PackageDefDeco(pdef: PackageDef): PackageDefAPI

  val PackageDef: PackageDefExtractor
  abstract class PackageDefExtractor {
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, PackageDef)]
  }

  // ----- Terms ----------------------------------------------------

  trait TermAPI {
    def tpe(implicit ctx: Context): Type
    def pos(implicit ctx: Context): Position
    def underlyingArgument(implicit ctx: Context): Term
    def underlying(implicit ctx: Context): Term
  }
  implicit def TermDeco(term: Term): TermAPI

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

    trait IdentAPI {
      def name(implicit ctx: Context): String
    }
    implicit def IdentDeco(ident: Ident): IdentAPI

    /** Scala term identifier */
    val Ident: IdentExtractor
    abstract class IdentExtractor {
      /** Matches a term identifier and returns its name */
      def unapply(tree: Tree)(implicit ctx: Context): Option[String]
    }

    val IsSelect: IsSelectModule
    abstract class IsSelectModule {
      /** Matches any Select and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Select]
    }

    trait SelectAPI {
      def qualifier(implicit ctx: Context): Term
      def name(implicit ctx: Context): String
      def signature(implicit ctx: Context): Option[Signature]
    }
    implicit def SelectDeco(select: Select): SelectAPI

    /** Scala term selection */
    val Select: SelectExtractor
    abstract class SelectExtractor {
      /** Matches `<qual: Term>.<name: String>: <sig: Option[Signature]>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, String, Option[Signature])]
    }

    val IsLiteral: IsLiteralModule
    abstract class IsLiteralModule {
      /** Matches any Literal and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Literal]
    }

    trait LiteralAPI {
      def constant(implicit ctx: Context): Constant
    }
    implicit def LiteralDeco(x: Literal): LiteralAPI

    /** Scala literal constant */
    val Literal: LiteralExtractor
    abstract class LiteralExtractor {
      def unapply(tree: Tree)(implicit ctx: Context): Option[Constant]
    }

    val IsThis: IsThisModule
    abstract class IsThisModule {
      /** Matches any This and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[This]
    }

    trait ThisAPI {
      def id(implicit ctx: Context): Option[Id]
    }
    implicit def ThisDeco(x: This): ThisAPI

    /** Scala `this` or `this[id]` */
    val This: ThisExtractor
    abstract class ThisExtractor {
      /** Matches new `this[<id: Option[Id]>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Option[Id]]
    }

    val IsNew: IsNewModule
    abstract class IsNewModule {
      /** Matches any New and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[New]
    }

    trait NewAPI {
      def tpt(implicit ctx: Context): TypeTree
    }
    implicit def NewDeco(x: New): NewAPI

    /** Scala `new` */
    val New: NewExtractor
    abstract class NewExtractor {
      /** Matches new `new <tpt: TypeTree>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[TypeTree]
    }

    val IsNamedArg: IsNamedArgModule
    abstract class IsNamedArgModule {
      /** Matches any NamedArg and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[NamedArg]
    }

    trait NamedArgAPI {
      def name(implicit ctx: Context): String
      def value(implicit ctx: Context): Term
    }
    implicit def NamedArgDeco(x: NamedArg): NamedArgAPI

    /** Scala named argument `x = y` in argument position */
    val NamedArg: NamedArgExtractor
    abstract class NamedArgExtractor {
      /** Matches `<name: String> = <value: Term>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(String, Term)]
    }

    val IsApply: IsApplyModule
    abstract class IsApplyModule {
      /** Matches any Apply and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Apply]
    }

    trait ApplyAPI {
      def fun(implicit ctx: Context): Term
      def args(implicit ctx: Context): List[Term]
    }
    implicit def ApplyDeco(x: Apply): ApplyAPI

    /** Scala parameter application */
    val Apply: ApplyExtractor
    abstract class ApplyExtractor {
      /** Matches function application `<fun: Term>(<args: List[Term]>)` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, List[Term])]
    }

    val IsTypeApply: IsTypeApplyModule
    abstract class IsTypeApplyModule {
      /** Matches any TypeApply and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[TypeApply]
    }

    trait TypeApplyAPI {
      def fun(implicit ctx: Context): Term
      def args(implicit ctx: Context): List[TypeTree]
    }
    implicit def TypeApplyDeco(x: TypeApply): TypeApplyAPI

    /** Scala type parameter application */
    val TypeApply: TypeApplyExtractor
    abstract class TypeApplyExtractor {
      /** Matches function type application `<fun: Term>[<args: List[TypeTree]>]` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, List[TypeTree])]
    }

    val IsSuper: IsSuperModule
    abstract class IsSuperModule {
      /** Matches any Super and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Super]
    }

    trait SuperAPI {
      def qualifier(implicit ctx: Context): Term
      def id(implicit ctx: Context): Option[Id]
    }
    implicit def SuperDeco(x: Super): SuperAPI

    /** Scala `x.super` or `x.super[id]` */
    val Super: SuperExtractor
    abstract class SuperExtractor {
      /** Matches new `<qualifier: Term>.super[<id: Option[Id]>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Option[Id])]
    }

    val IsTyped: IsTypedModule
    abstract class IsTypedModule {
      /** Matches any Typed and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Typed]
    }

    trait TypedAPI {
      def expr(implicit ctx: Context): Term
      def tpt(implicit ctx: Context): Term
    }
    implicit def TypedDeco(x: Typed): TypedAPI

    /** Scala ascription `x: T` */
    val Typed: TypedExtractor
    abstract class TypedExtractor {
      /** Matches `<expr: Term>: <tpt: Term>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, TypeTree)]
    }

    val IsAssign: IsAssignModule
    abstract class IsAssignModule {
      /** Matches any Assign and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Assign]
    }

    trait AssignAPI {
      def lhs(implicit ctx: Context): Term
      def rhs(implicit ctx: Context): Term
    }
    implicit def AssignDeco(x: Assign): AssignAPI

    /** Scala assign `x = y` */
    val Assign: AssignExtractor
    abstract class AssignExtractor {
      /** Matches `<lhs: Term> = <rhs: Term>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Term)]
    }

    val IsBlock: IsBlockModule
    abstract class IsBlockModule {
      /** Matches any Block and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Block]
    }

    trait BlockAPI {
      def statements(implicit ctx: Context): List[Statement]
      def expr(implicit ctx: Context): Term
    }
    implicit def BlockDeco(x: Block): BlockAPI

    /** Scala code block `{ stat0; ...; statN; expr }` term */
    val Block: BlockExtractor
    abstract class BlockExtractor {
      /** Matches `{ <statements: List[Statement]>; <expr: Term> }` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(List[Statement], Term)]
    }

    val IsLambda: IsLambdaModule
    abstract class IsLambdaModule {
      /** Matches any Lambda and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Lambda]
    }

    trait LambdaAPI {
      def meth(implicit ctx: Context): Term
      def tptOpt(implicit ctx: Context): Option[TypeTree]
    }
    implicit def LambdaDeco(x: Lambda): LambdaAPI

    val Lambda: LambdaExtractor
    abstract class LambdaExtractor {
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Option[TypeTree])]
    }

    val IsIf: IsIfModule
    abstract class IsIfModule {
      /** Matches any If and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[If]
    }

    trait IfAPI {
      def cond(implicit ctx: Context): Term
      def thenp(implicit ctx: Context): Term
      def elsep(implicit ctx: Context): Term
    }
    implicit def IfDeco(x: If): IfAPI

    /** Scala `if`/`else` term */
    val If: IfExtractor
    abstract class IfExtractor {
      /** Matches `if (<cond: Term>) <thenp: Term> else <elsep: Term>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Term, Term)]
    }

    val IsMatch: IsMatchModule
    abstract class IsMatchModule {
      /** Matches any Match and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Match]
    }

    trait MatchAPI {
      def scrutinee(implicit ctx: Context): Term
      def cases(implicit ctx: Context): List[CaseDef]
    }
    implicit def MatchDeco(x: Match): MatchAPI

    /** Scala `match` term */
    val Match: MatchExtractor
    abstract class MatchExtractor {
      /** Matches `<scrutinee: Trem> match { <cases: List[CaseDef]> }` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, List[CaseDef])]
    }

    val IsTry: IsTryModule
    abstract class IsTryModule {
      /** Matches any Try and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Try]
    }

    trait TryAPI {
      def body(implicit ctx: Context): Term
      def cases(implicit ctx: Context): List[CaseDef]
      def finalizer(implicit ctx: Context): Option[Term]
    }
    implicit def TryDeco(x: Try): TryAPI

    /** Scala `try`/`catch`/`finally` term */
    val Try: TryExtractor
    abstract class TryExtractor {
      /** Matches `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, List[CaseDef], Option[Term])]
    }

    val IsReturn: IsReturnModule
    abstract class IsReturnModule {
      /** Matches any Return and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Return]
    }

    trait ReturnAPI {
      def expr(implicit ctx: Context): Term
    }
    implicit def ReturnDeco(x: Return): ReturnAPI

    /** Scala local `return` */
    val Return: ReturnExtractor
    abstract class ReturnExtractor {
      /** Matches `return <expr: Term>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Term]
    }

    val IsRepeated: IsRepeatedModule
    abstract class IsRepeatedModule {
      /** Matches any Repeated and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Repeated]
    }

    trait RepeatedAPI {
      def elems(implicit ctx: Context): List[Term]
    }
    implicit def RepeatedDeco(x: Repeated): RepeatedAPI

    val Repeated: RepeatedExtractor
    abstract class RepeatedExtractor {
      def unapply(tree: Tree)(implicit ctx: Context): Option[List[Term]]
    }

    val IsInlined: IsInlinedModule
    abstract class IsInlinedModule {
      /** Matches any Inlined and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Inlined]
    }

    trait InlinedAPI {
      def call(implicit ctx: Context): Option[TermOrTypeTree]
      def bindings(implicit ctx: Context): List[Definition]
      def body(implicit ctx: Context): Term
    }
    implicit def InlinedDeco(x: Inlined): InlinedAPI

    val Inlined: InlinedExtractor
    abstract class InlinedExtractor {
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Option[TermOrTypeTree], List[Definition], Term)]
    }

    val IsSelectOuter: IsSelectOuterModule
    abstract class IsSelectOuterModule {
      /** Matches any SelectOuter and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[SelectOuter]
    }

    trait SelectOuterAPI {
      def qualifier(implicit ctx: Context): Term
      def level(implicit ctx: Context): Int
      def tpe(implicit ctx: Context): Type
    }
    implicit def SelectOuterDeco(x: SelectOuter): SelectOuterAPI

    val SelectOuter: SelectOuterExtractor
    abstract class SelectOuterExtractor {
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Int, Type)]
    }

    val IsWhile: IsWhileModule
    abstract class IsWhileModule {
      /** Matches any While and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[While]
    }

    trait WhileAPI {
      def cond(implicit ctx: Context): Term
      def body(implicit ctx: Context): Term
    }
    implicit def WhileDeco(x: While): WhileAPI

    val While: WhileExtractor
    abstract class WhileExtractor {
      /** Extractor for while loops. Matches `while (<cond>) <body>` and returns (<cond>, <body>) */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Term)]
    }
  }

  implicit def termAsTermOrTypeTree(term: Term): TermOrTypeTree
}
