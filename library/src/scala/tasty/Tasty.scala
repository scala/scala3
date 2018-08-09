package scala.tasty

import scala.tasty.reflect.StandardDefinitions
import scala.tasty.util.Show

abstract class Tasty extends StandardDefinitions { tasty =>

  // ===== Quotes ===================================================

  trait QuotedExprAPI {
    def toTasty(implicit ctx: Context): Term
  }
  implicit def QuotedExprDeco[T](expr: quoted.Expr[T]): QuotedExprAPI

  trait QuotedTypeAPI {
    def toTasty(implicit ctx: Context): TypeTree
  }
  implicit def QuotedTypeDeco[T](tpe: quoted.Type[T]): QuotedTypeAPI

  // ===== Show =====================================================

  implicit def defaultShow: Show[tasty.type]

  def showExtractors: Show[tasty.type]

  def showSourceCode: Show[tasty.type]

  // ===== Contexts =================================================

  type Context

  trait ContextAPI {
    def owner: Definition

    /** Returns the source file being compiled. The path is relative to the current working directory. */
    def source: java.nio.file.Path
  }
  implicit def ContextDeco(ctx: Context): ContextAPI

  implicit def rootContext: Context

  /** Root position of this tasty context. For macros it corresponds to the expansion site. */
  def rootPosition: Position

  // ===== Id =======================================================

  type Id

  trait IdAPI extends Positioned {
    def name(implicit ctx: Context): String
  }
  implicit def IdDeco(id: Id): IdAPI

  val Id: IdExtractor
  abstract class IdExtractor {
    def unapply(id: Id): Option[String]
  }

  // ===== Trees ====================================================

  type Tree

  trait TreeAPI extends Positioned {
    def show(implicit ctx: Context, s: Show[tasty.type]): String
  }
  implicit def TreeDeco(tree: Tree): TreeAPI

  type PackageClause <: Tree

  val IsPackageClause: IsPackageClauseExtractor
  abstract class IsPackageClauseExtractor {
    def unapply(tree: Tree)(implicit ctx: Context): Option[PackageClause]
  }

  val PackageClause: PackageClauseExtractor
  abstract class PackageClauseExtractor {
    def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, List[Tree])]
  }

  trait PackageClauseAPI {
    def definition(implicit ctx: Context): Definition
  }
  implicit def PackageClauseDeco(pack: PackageClause): PackageClauseAPI

  // ----- Statements -----------------------------------------------

  type Statement <: Tree

  type Import <: Statement

  val Import: ImportExtractor
  abstract class ImportExtractor {
    def unapply(imp: Tree)(implicit ctx: Context): Option[(Term, List[ImportSelector])]
  }

  trait ImportAPI {
    def expr(implicit ctx: Context): Term
    def selector(implicit ctx: Context): List[ImportSelector]
  }
  implicit def ImportDeco(imp: Import): ImportAPI

  type ImportSelector

  val SimpleSelector: SimpleSelectorExtractor
  abstract class SimpleSelectorExtractor {
    def unapply(importSelector: ImportSelector)(implicit ctx: Context): Option[Id]
  }

  val RenameSelector: RenameSelectorExtractor
  abstract class RenameSelectorExtractor {
    def unapply(importSelector: ImportSelector)(implicit ctx: Context): Option[(Id, Id)]
  }

  val OmitSelector: OmitSelectorExtractor
  abstract class OmitSelectorExtractor {
    def unapply(importSelector: ImportSelector)(implicit ctx: Context): Option[Id]
  }

  // ----- Definitions ----------------------------------------------

  type Definition <: Statement

  val IsDefinition: IsDefinitionExtractor
  abstract class IsDefinitionExtractor {
    def unapply(tree: Tree)(implicit ctx: Context): Option[Definition]
  }

  trait DefinitionAPI {
    def name(implicit ctx: Context): String
    def flags(implicit ctx: Context): FlagSet
    def privateWithin(implicit ctx: Context): Option[Type]
    def protectedWithin(implicit ctx: Context): Option[Type]
    def annots(implicit ctx: Context): List[Term]
    def owner(implicit ctx: Context): Definition
    def localContext(implicit ctx: Context): Context
  }
  implicit def DefinitionDeco(definition: Definition): DefinitionAPI

  // ClassDef

  type ClassDef <: Definition

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

  type DefDef <: Definition

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

  type ValDef <: Definition

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

  type TypeDef <: Definition

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

  type PackageDef <: Definition

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

  // ----- Parents --------------------------------------------------

  type Parent /* Term | TypeTree */

  // ----- Terms ----------------------------------------------------

  type Term <: Statement with Parent

  trait TermAPI extends Typed with Positioned {
    def toExpr[T: quoted.Type](implicit ctx: Context): quoted.Expr[T]
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

  // ----- CaseDef --------------------------------------------------

  type CaseDef

  trait CaseDefAPI {
    def show(implicit ctx: Context, s: Show[tasty.type]): String
    def pattern(implicit ctx: Context): Pattern
    def guard(implicit ctx: Context): Option[Term]
    def rhs(implicit ctx: Context): Term
  }
  implicit def CaseDefDeco(caseDef: CaseDef): CaseDefAPI

  val CaseDef: CaseDefExtractor
  abstract class CaseDefExtractor {
    def unapply(x: CaseDef): Option[(Pattern, Option[Term], Term)]
  }

  // ----- Patterns -------------------------------------------------

  type Pattern

  trait PatternAPI extends Typed with Positioned {
    def show(implicit ctx: Context, s: Show[tasty.type]): String

  }
  implicit def PatternDeco(pattern: Pattern): PatternAPI

  val Pattern: PatternModule
  abstract class PatternModule {

    val Value: ValueExtractor
    abstract class ValueExtractor {
      def unapply(pattern: Pattern)(implicit ctx: Context): Option[Term]
    }

    val Bind: BindExtractor
    abstract class BindExtractor {
      def unapply(pattern: Pattern)(implicit ctx: Context): Option[(String, Pattern)]
    }

    val Unapply: UnapplyExtractor
    abstract class UnapplyExtractor {
      def unapply(pattern: Pattern)(implicit ctx: Context): Option[(Term, List[Term], List[Pattern])]
    }

    val Alternative: AlternativeExtractor
    abstract class AlternativeExtractor {
      def unapply(pattern: Pattern)(implicit ctx: Context): Option[List[Pattern]]
    }

    val TypeTest: TypeTestExtractor
    abstract class TypeTestExtractor {
      def unapply(pattern: Pattern)(implicit ctx: Context): Option[TypeTree]
    }

  }

  // ----- TypeTrees ------------------------------------------------

  type TypeOrBoundsTree

  trait TypeOrBoundsTreeAPI {
    def show(implicit ctx: Context, s: Show[tasty.type]): String
    def tpe(implicit ctx: Context): TypeOrBounds
  }
  implicit def TypeOrBoundsTreeDeco(tpt: TypeOrBoundsTree): TypeOrBoundsTreeAPI


  // ----- TypeTrees ------------------------------------------------

  type TypeTree <: TypeOrBoundsTree

  trait TypeTreeAPI extends Typed with Positioned
  implicit def TypeTreeDeco(tpt: TypeTree): TypeTreeAPI

  val IsTypeTree: IsTypeTreeExtractor
  abstract class IsTypeTreeExtractor {
    def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeTree]
    def unapply(parent: Parent)(implicit ctx: Context, dummy: DummyImplicit): Option[TypeTree]
  }

  val TypeTree: TypeTreeModule
  abstract class TypeTreeModule {

    /** TypeTree containing an inferred type */
    val Synthetic: SyntheticExtractor
    abstract class SyntheticExtractor {
      /** Matches a TypeTree containing an inferred type */
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Boolean
    }

    val TypeIdent: TypeIdentExtractor
    abstract class TypeIdentExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[String]
    }

    val TermSelect: TermSelectExtractor
    abstract class TermSelectExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(Term, String)]
    }

    val TypeSelect: TypeSelectExtractor
    abstract class TypeSelectExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, String)]
    }

    val Singleton: SingletonExtractor
    abstract class SingletonExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[Term]
    }

    val Refined: RefinedExtractor
    abstract class RefinedExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, List[Definition])]
    }

    val Applied: AppliedExtractor
    abstract class AppliedExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, List[TypeOrBoundsTree])]
    }

    val Annotated: AnnotatedExtractor
    abstract class AnnotatedExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, Term)]
    }

    val And: AndExtractor
    abstract class AndExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, TypeTree)]
    }

    val Or: OrExtractor
    abstract class OrExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, TypeTree)]
    }

    val ByName: ByNameExtractor
    abstract class ByNameExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeTree]
    }

    val TypeLambdaTree: TypeLambdaTreeExtractor
    abstract class TypeLambdaTreeExtractor {
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(List[TypeDef], TypeOrBoundsTree)]
    }

    val Bind: BindExtractor
    abstract class BindExtractor{
      def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(String, TypeBoundsTree)]
    }
  }

  // ----- TypeBoundsTrees ------------------------------------------------

  type TypeBoundsTree <: TypeOrBoundsTree

  trait TypeBoundsTreeAPI {
    def tpe(implicit ctx: Context): TypeBounds
    def low(implicit ctx: Context): TypeTree
    def hi(implicit ctx: Context): TypeTree
  }
  implicit def TypeBoundsTreeDeco(tpt: TypeBoundsTree): TypeBoundsTreeAPI

  val IsTypeBoundsTree: IsTypeBoundsTreeExtractor
  abstract class IsTypeBoundsTreeExtractor {
    def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeBoundsTree]
  }

  val TypeBoundsTree: TypeBoundsTreeExtractor
  abstract class TypeBoundsTreeExtractor {
    def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, TypeTree)]
  }

  /** TypeBoundsTree containing an inferred type bounds */
  val SyntheticBounds: SyntheticBoundsExtractor
  abstract class SyntheticBoundsExtractor {
    /** Matches a TypeBoundsTree containing inferred type bounds */
    def unapply(typeOrBoundsTree: TypeOrBoundsTree)(implicit ctx: Context): Boolean
  }

  // ===== Types ====================================================

  type TypeOrBounds

  trait Typed {
    def tpe(implicit ctx: Context): Type
  }

  trait TypeOrBoundsAPI {
    def show(implicit ctx: Context, s: Show[tasty.type]): String
  }
  implicit def TypeOrBoundsDeco(tpe: TypeOrBounds): TypeOrBoundsAPI

  // ----- Types ----------------------------------------------------

  type Type <: TypeOrBounds

  trait TypeAPI {
    def =:=(other: Type)(implicit ctx: Context): Boolean
    def <:<(other: Type)(implicit ctx: Context): Boolean
  }
  implicit def TypeDeco(tpe: Type): TypeAPI

  type RecursiveType <: Type

  type LambdaType[ParamInfo <: TypeOrBounds] <: Type
  type MethodType <: LambdaType[Type]
  type PolyType <: LambdaType[TypeBounds]
  type TypeLambda <: LambdaType[TypeBounds]

  trait MethodTypeAPI {
    def isImplicit: Boolean
    def isErased: Boolean
    def paramNames(implicit ctx: Context): List[String]
    def paramTypes(implicit ctx: Context): List[Type]
    def resultTpe(implicit ctx: Context): Type
  }
  implicit def MethodTypeDeco(tpt: MethodType): MethodTypeAPI

  trait PolyTypeAPI {
    def paramNames(implicit ctx: Context): List[String]
    def paramTypes(implicit ctx: Context): List[TypeBounds]
    def resultTpe(implicit ctx: Context): Type
  }
  implicit def PolyTypeDeco(tpt: PolyType): PolyTypeAPI

  trait TypeLambdaAPI {
    def paramNames(implicit ctx: Context): List[String]
    def paramTypes(implicit ctx: Context): List[TypeBounds]
    def resultTpe(implicit ctx: Context): Type
  }
  implicit def TypeLambdaDeco(tpt: TypeLambda): TypeLambdaAPI

  val IsType: IsTypeExtractor
  abstract class IsTypeExtractor {
    def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Type]
  }

  val Type: TypeModule
  abstract class TypeModule {

    val ConstantType: ConstantTypeExtractor
    abstract class ConstantTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Constant]
    }

    val SymRef: SymRefExtractor
    abstract class SymRefExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Definition, TypeOrBounds /* Type | NoPrefix */)]
    }

    val TermRef: TermRefExtractor
    abstract class TermRefExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(String, TypeOrBounds /* Type | NoPrefix */)]
    }

    val TypeRef: TypeRefExtractor
    abstract class TypeRefExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(String, TypeOrBounds /* Type | NoPrefix */)]
    }

    val SuperType: SuperTypeExtractor
    abstract class SuperTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)]
    }

    val Refinement: RefinementExtractor
    abstract class RefinementExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, String, TypeOrBounds /* Type | TypeBounds */)]
    }

    val AppliedType: AppliedTypeExtractor
    abstract class AppliedTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, List[TypeOrBounds /* Type | TypeBounds */])]
    }

    val AnnotatedType: AnnotatedTypeExtractor
    abstract class AnnotatedTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Term)]
    }

    val AndType: AndTypeExtractor
    abstract class AndTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)]
    }

    val OrType: OrTypeExtractor
    abstract class OrTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)]
    }

    val ByNameType: ByNameTypeExtractor
    abstract class ByNameTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Type]
    }

    val ParamRef: ParamRefExtractor
    abstract class ParamRefExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(LambdaType[TypeOrBounds], Int)]
    }

    val ThisType: ThisTypeExtractor
    abstract class ThisTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Type]
    }

    val RecursiveThis: RecursiveThisExtractor
    abstract class RecursiveThisExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[RecursiveType]
    }

    val RecursiveType: RecursiveTypeExtractor
    abstract class RecursiveTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[Type]
    }

    val MethodType: MethodTypeExtractor
    abstract class MethodTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(List[String], List[Type], Type)]
    }

    val PolyType: PolyTypeExtractor
    abstract class PolyTypeExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(List[String], List[TypeBounds], Type)]
    }

    val TypeLambda: TypeLambdaExtractor
    abstract class TypeLambdaExtractor {
      def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(List[String], List[TypeBounds], Type)]
    }

  }

  // ----- TypeBounds -----------------------------------------------

  type TypeBounds <: TypeOrBounds

  val IsTypeBounds: IsTypeBoundsExtractor
  abstract class IsTypeBoundsExtractor {
    def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[TypeBounds]
  }

  val TypeBounds: TypeBoundsExtractor
  abstract class TypeBoundsExtractor {
    def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Option[(Type, Type)]
  }

  trait TypeBoundsAPI {
    def low(implicit ctx: Context): Type
    def hi(implicit ctx: Context): Type
  }
  implicit def TypeBoundsDeco(bounds: TypeBounds): TypeBoundsAPI

  // ----- NoPrefix -------------------------------------------------

  type NoPrefix <: TypeOrBounds

  val NoPrefix: NoPrefixExtractor
  abstract class NoPrefixExtractor {
    def unapply(typeOrBounds: TypeOrBounds)(implicit ctx: Context): Boolean
  }

  // ===== Constants ================================================

  type Constant
  trait ConstantAPI {
    def show(implicit ctx: Context, s: Show[tasty.type]): String
    def value: Any
  }
  implicit def ConstantDeco(const: Constant): ConstantAPI

  val Constant: ConstantModule
  abstract class ConstantModule {

    val Unit: UnitExtractor
    abstract class UnitExtractor {
      def unapply(constant: Constant): Boolean
    }

    val Null: NullExtractor
    abstract class NullExtractor {
      def unapply(constant: Constant): Boolean
    }

    val Boolean: BooleanExtractor
    abstract class BooleanExtractor {
      def unapply(constant: Constant): Option[Boolean]
    }

    val Byte: ByteExtractor
    abstract class ByteExtractor {
      def unapply(constant: Constant): Option[Byte]
    }

    val Short: ShortExtractor
    abstract class ShortExtractor {
      def unapply(constant: Constant): Option[Short]
    }

    val Char: CharExtractor
    abstract class CharExtractor {
      def unapply(constant: Constant): Option[Char]
    }

    val Int: IntExtractor
    abstract class IntExtractor {
      def unapply(constant: Constant): Option[Int]
    }

    val Long: LongExtractor
    abstract class LongExtractor {
      def unapply(constant: Constant): Option[Long]
    }

    val Float: FloatExtractor
    abstract class FloatExtractor {
      def unapply(constant: Constant): Option[Float]
    }

    val Double: DoubleExtractor
    abstract class DoubleExtractor {
      def unapply(constant: Constant): Option[Double]
    }

    val String: StringExtractor
    abstract class StringExtractor {
      def unapply(constant: Constant): Option[String]
    }

    val ClassTag: ClassTagExtractor
    abstract class ClassTagExtractor {
      def unapply(constant: Constant): Option[Type]
    }

    /** Extractor for scala.Symbol literals */
    val Symbol: SymbolExtractor
    /** Extractor for scala.Symbol literals */
    abstract class SymbolExtractor {
      def unapply(constant: Constant): Option[scala.Symbol]
    }
  }

  // ===== Signature ================================================

  type Signature

  val Signature: SignatureExtractor
  abstract class SignatureExtractor {
    def unapply(sig: Signature)(implicit ctx: Context): Option[(List[String], String)]
  }

  trait SignatureAPI {
    def paramSigs: List[String]
    def resultSig: String
  }
  implicit def SignatureDeco(sig: Signature): SignatureAPI

  // ===== Positions ================================================

  type Position

  trait PositionAPI {
    def start: Int
    def end: Int

    def sourceFile: java.nio.file.Path

    def startLine: Int
    def startColumn: Int
    def endLine: Int
    def endColumn: Int
  }
  implicit def PositionDeco(pos: Position): PositionAPI

  trait Positioned {
    def pos(implicit ctx: Context): Position
  }

}
