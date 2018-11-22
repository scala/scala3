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

    /** Scala term selection */
    val Select: SelectExtractor
    abstract class SelectExtractor {
      /** Matches `<qual: Term>.<name: String>: <sig: Signature>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, String, Option[Signature])]
    }

    val IsLiteral: IsLiteralModule
    abstract class IsLiteralModule {
      /** Matches any Literal and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Literal]
    }

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

    /** Scala ascription `x: T` */
    val Typed: TypedExtractor
    abstract class TypedExtractor {
      /** Matches `<x: Term>: <tpt: Term>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, TypeTree)]
    }

    val IsAssign: IsAssignModule
    abstract class IsAssignModule {
      /** Matches any Assign and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Assign]
    }

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

    val Lambda: LambdaExtractor
    abstract class LambdaExtractor {
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Option[TypeTree])]
    }

    val IsIf: IsIfModule
    abstract class IsIfModule {
      /** Matches any If and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[If]
    }

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

    val Repeated: RepeatedExtractor
    abstract class RepeatedExtractor {
      def unapply(tree: Tree)(implicit ctx: Context): Option[List[Term]]
    }

    val IsInlined: IsInlinedModule
    abstract class IsInlinedModule {
      /** Matches any Inlined and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Inlined]
    }

    val Inlined: InlinedExtractor
    abstract class InlinedExtractor {
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Option[TermOrTypeTree], List[Definition], Term)]
    }

    val IsSelectOuter: IsSelectOuterModule
    abstract class IsSelectOuterModule {
      /** Matches any SelectOuter and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[SelectOuter]
    }

    val SelectOuter: SelectOuterExtractor
    abstract class SelectOuterExtractor {
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Int, Type)]
    }

    val IsWhile: IsWhileModule
    abstract class IsWhileModule {
      /** Matches any While and returns it */
      def unapply(tree: Tree)(implicit ctx: Context): Option[While]
    }

    val While: WhileExtractor
    abstract class WhileExtractor {
      /** Extractor for while loops. Matches `while (<cond>) <body>` and returns (<cond>, <body>) */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Term)]
    }
  }

  implicit def termAsTermOrTypeTree(term: Term): TermOrTypeTree
}
