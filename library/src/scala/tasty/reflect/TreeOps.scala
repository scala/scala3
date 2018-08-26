package scala.tasty
package reflect

trait TreeOps extends TastyCore {

  trait TreeAPI {
    def pos(implicit ctx: Context): Position
    def symbol(implicit ctx: Context): Symbol
  }
  implicit def TreeDeco(tree: Tree): TreeAPI

  val IsPackageClause: IsPackageClauseExtractor
  abstract class IsPackageClauseExtractor {
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

  val IsDefinition: IsDefinitionExtractor
  abstract class IsDefinitionExtractor {
    def unapply(tree: Tree)(implicit ctx: Context): Option[Definition]
  }

  trait DefinitionAPI {
    def name(implicit ctx: Context): String
  }
  implicit def DefinitionDeco(definition: Definition): DefinitionAPI

  // ClassDef

  val IsClassDef: IsClassDefExtractor
  abstract class IsClassDefExtractor {
    def unapply(tree: Tree)(implicit ctx: Context): Option[ClassDef]
  }

  val ClassDef: ClassDefExtractor
  abstract class ClassDefExtractor {
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, DefDef, List[Parent], Option[ValDef], List[Statement])]
  }

  trait ClassDefAPI {
    def constructor(implicit ctx: Context): DefDef
    def parents(implicit ctx: Context): List[Parent]
    def self(implicit ctx: Context): Option[ValDef]
    def body(implicit ctx: Context): List[Statement]
  }
  implicit def ClassDefDeco(cdef: ClassDef): ClassDefAPI

  // DefDef

  val IsDefDef: IsDefDefExtractor
  abstract class IsDefDefExtractor {
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
  }
  implicit def DefDefDeco(ddef: DefDef): DefDefAPI

  // ValDef

  val IsValDef: IsValDefExtractor
  abstract class IsValDefExtractor {
    def unapply(tree: Tree)(implicit ctx: Context): Option[ValDef]
  }

  val ValDef: ValDefExtractor
  abstract class ValDefExtractor {
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, TypeTree, Option[Term])]
  }

  trait ValDefAPI {
    def tpt(implicit ctx: Context): TypeTree
    def rhs(implicit ctx: Context): Option[Term]
  }
  implicit def ValDefDeco(vdef: ValDef): ValDefAPI

  // TypeDef

  val IsTypeDef: IsTypeDefExtractor
  abstract class IsTypeDefExtractor {
    def unapply(tree: Tree)(implicit ctx: Context): Option[TypeDef]
  }

  val TypeDef: TypeDefExtractor
  abstract class TypeDefExtractor {
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, TypeOrBoundsTree /* TypeTree | TypeBoundsTree */)]
  }

  trait TypeDefAPI {
    def rhs(implicit ctx: Context): TypeOrBoundsTree
  }
  implicit def TypeDefDeco(tdef: TypeDef): TypeDefAPI

  // PackageDef

  val IsPackageDef: IsPackageDefExtractor
  abstract class IsPackageDefExtractor {
    def unapply(tree: Tree)(implicit ctx: Context): Option[PackageDef]
  }

  trait PackageDefAPI {
    def owner(implicit ctx: Context): PackageDef
    def members(implicit ctx: Context): List[Statement]
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
  }
  implicit def TermDeco(term: Term): TermAPI

  val IsTerm: IsTermExtractor
  abstract class IsTermExtractor {
    /** Matches any term */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Term]
    /** Matches any term */
    def unapply(parent: Parent)(implicit ctx: Context, dummy: DummyImplicit): Option[Term]
  }

  /** Scala term. Any tree that can go in expression position. */
  val Term: TermModule
  abstract class TermModule {

    /** Scala term identifier */
    val Ident: IdentExtractor
    abstract class IdentExtractor {
      /** Matches a term identifier and returns its name */
      def unapply(tree: Tree)(implicit ctx: Context): Option[String]
    }

    /** Scala term selection */
    val Select: SelectExtractor
    abstract class SelectExtractor {
      /** Matches `<qual: Term>.<name: String>: <sig: Signature>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, String, Option[Signature])]
    }

    /** Scala literal constant */
    val Literal: LiteralExtractor
    abstract class LiteralExtractor {
      def unapply(tree: Tree)(implicit ctx: Context): Option[Constant]
    }

    /** Scala `this` or `this[id]` */
    val This: ThisExtractor
    abstract class ThisExtractor {
      /** Matches new `this[<id: Option[Id]>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Option[Id]]
    }

    /** Scala `new` */
    val New: NewExtractor
    abstract class NewExtractor {
      /** Matches new `new <tpt: TypeTree>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[TypeTree]
    }

    /** Scala named argument `x = y` in argument position */
    val NamedArg: NamedArgExtractor
    abstract class NamedArgExtractor {
      /** Matches `<name: String> = <value: Term>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(String, Term)]
    }

    /** Scala parameter application */
    val Apply: ApplyExtractor
    abstract class ApplyExtractor {
      /** Matches function application `<fun: Term>(<args: List[Term]>)` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, List[Term])]
    }

    /** Scala type parameter application */
    val TypeApply: TypeApplyExtractor
    abstract class TypeApplyExtractor {
      /** Matches function type application `<fun: Term>[<args: List[TypeTree]>]` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, List[TypeTree])]
    }

    /** Scala `x.super` or `x.super[id]` */
    val Super: SuperExtractor
    abstract class SuperExtractor {
      /** Matches new `<qualifier: Term>.super[<id: Option[Id]>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Option[Id])]
    }

    /** Scala ascription `x: T` */
    val Typed: TypedExtractor
    abstract class TypedExtractor {
      /** Matches `<x: Term>: <tpt: Term>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, TypeTree)]
    }

    /** Scala assign `x = y` */
    val Assign: AssignExtractor
    abstract class AssignExtractor {
      /** Matches `<lhs: Term> = <rhs: Term>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Term)]
    }

    /** Scala code block `{ stat0; ...; statN; expr }` term */
    val Block: BlockExtractor
    abstract class BlockExtractor {
      /** Matches `{ <statements: List[Statement]>; <expr: Term> }` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(List[Statement], Term)]
    }

    val Lambda: LambdaExtractor
    abstract class LambdaExtractor {
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Option[TypeTree])]
    }

    /** Scala `if`/`else` term */
    val If: IfExtractor
    abstract class IfExtractor {
      /** Matches `if (<cond: Term>) <thenp: Term> else <elsep: Term>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Term, Term)]
    }

    /** Scala `match` term */
    val Match: MatchExtractor
    abstract class MatchExtractor {
      /** Matches `<scrutinee: Trem> match { <cases: List[CaseDef]> }` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, List[CaseDef])]
    }

    /** Scala `try`/`catch`/`finally` term */
    val Try: TryExtractor
    abstract class TryExtractor {
      /** Matches `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, List[CaseDef], Option[Term])]
    }

    /** Scala local `return` */
    val Return: ReturnExtractor
    abstract class ReturnExtractor {
      /** Matches `return <expr: Term>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Term]
    }

    val Repeated: RepeatedExtractor
    abstract class RepeatedExtractor {
      def unapply(tree: Tree)(implicit ctx: Context): Option[List[Term]]
    }

    val Inlined: InlinedExtractor
    abstract class InlinedExtractor {
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Option[Term], List[Definition], Term)]
    }

    val SelectOuter: SelectOuterExtractor
    abstract class SelectOuterExtractor {
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Int, Type)]
    }

    val While: WhileExtractor
    abstract class WhileExtractor {
      /** Extractor for while loops. Matches `while (<cond>) <body>` and returns (<cond>, <body>) */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Term)]
    }

    val DoWhile: DoWhileExtractor
    abstract class DoWhileExtractor {
      /** Extractor for do while loops. Matches `do <body> while (<cond>)` and returns (<body>, <cond>) */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Term)]
    }
  }

  implicit def termAsParent(term: Term): Parent
}
