package scala.tasty

import scala.reflect.ClassTag
import scala.tasty.util.Show

abstract class Tasty { tasty =>

  // ===== Quotes ===================================================

  trait QuotedExprAPI {
    def toTasty(implicit ctx: Context): Term
  }
  implicit def QuotedExprDeco[T](x: quoted.Expr[T]): QuotedExprAPI

  trait QuotedTypeAPI {
    def toTasty(implicit ctx: Context): TypeTree
  }
  implicit def QuotedTypeDeco[T](x: quoted.Type[T]): QuotedTypeAPI

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

  implicit def idClassTag: ClassTag[Id]

  val Id: IdExtractor
  abstract class IdExtractor {
    def unapply(x: Id): Option[String]
  }

  // ===== Trees ====================================================

  type Tree

  trait TreeAPI extends Positioned {
    def show(implicit ctx: Context, s: Show[tasty.type]): String
  }
  implicit def TreeDeco(tree: Tree): TreeAPI

  type PackageClause <: Tree

  implicit def packageClauseClassTag: ClassTag[PackageClause]

  val PackageClause: PackageClauseExtractor
  abstract class PackageClauseExtractor {
    def unapply(x: PackageClause)(implicit ctx: Context): Option[(Term, List[Tree])]
  }

  trait PackageClauseAPI {
    def definition(implicit ctx: Context): Definition
  }
  implicit def PackageClauseDeco(pack: PackageClause): PackageClauseAPI

  // ----- Statements -----------------------------------------------

  type Statement <: Tree

  type Import <: Statement

  implicit def importClassTag: ClassTag[Import]

  val Import: ImportExtractor
  abstract class ImportExtractor {
    def unapply(x: Import)(implicit ctx: Context): Option[(Term, List[ImportSelector])]
  }

  trait ImportAPI {
    def expr(implicit ctx: Context): Term
    def selector(implicit ctx: Context): List[ImportSelector]
  }
  implicit def ImportDeco(imp: Import): ImportAPI

  type ImportSelector

  implicit def importSelectorClassTag: ClassTag[ImportSelector]

  val SimpleSelector: SimpleSelectorExtractor
  abstract class SimpleSelectorExtractor {
    def unapply(x: ImportSelector)(implicit ctx: Context): Option[Id]
  }

  val RenameSelector: RenameSelectorExtractor
  abstract class RenameSelectorExtractor {
    def unapply(x: ImportSelector)(implicit ctx: Context): Option[(Id, Id)]
  }

  val OmitSelector: OmitSelectorExtractor
  abstract class OmitSelectorExtractor {
    def unapply(x: ImportSelector)(implicit ctx: Context): Option[Id]
  }

  // ----- Definitions ----------------------------------------------

  type Definition <: Statement

  val Definition: DefinitionExtractor
  abstract class DefinitionExtractor {
    def unapply(x: Definition)(implicit ctx: Context): Boolean
  }

  implicit def definitionClassTag: ClassTag[Definition]

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

  implicit def classDefClassTag: ClassTag[ClassDef]

  val ClassDef: ClassDefExtractor
  abstract class ClassDefExtractor {
    def unapply(x: ClassDef)(implicit ctx: Context): Option[(String, DefDef, List[Parent], Option[ValDef], List[Statement])]
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

  implicit def defDefClassTag: ClassTag[DefDef]

  val DefDef: DefDefExtractor
  abstract class DefDefExtractor {
    def unapply(x: DefDef)(implicit ctx: Context): Option[(String, List[TypeDef],  List[List[ValDef]], TypeTree, Option[Term])]
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

  implicit def valDefClassTag: ClassTag[ValDef]

  val ValDef: ValDefExtractor
  abstract class ValDefExtractor {
    def unapply(x: ValDef)(implicit ctx: Context): Option[(String, TypeTree, Option[Term])]
  }

  trait ValDefAPI {
    def tpt(implicit ctx: Context): TypeTree
    def rhs(implicit ctx: Context): Option[Term]
  }
  implicit def ValDefDeco(vdef: ValDef): ValDefAPI

  // TypeDef

  type TypeDef <: Definition

  implicit def typeDefClassTag: ClassTag[TypeDef]

  val TypeDef: TypeDefExtractor
  abstract class TypeDefExtractor {
    def unapply(x: TypeDef)(implicit ctx: Context): Option[(String, TypeOrBoundsTree /* TypeTree | TypeBoundsTree */)]
  }

  trait TypeDefAPI {
    def rhs(implicit ctx: Context): TypeOrBoundsTree
  }
  implicit def TypeDefDeco(tdef: TypeDef): TypeDefAPI

  // PackageDef

  type PackageDef <: Definition

  trait PackageDefAPI {
    def owner(implicit ctx: Context): PackageDef
    def members(implicit ctx: Context): List[Statement]
  }
  implicit def PackageDefDeco(pdef: PackageDef): PackageDefAPI

  implicit def packageDefClassTag: ClassTag[PackageDef]

  val PackageDef: PackageDefExtractor
  abstract class PackageDefExtractor {
    def unapply(x: PackageDef)(implicit ctx: Context): Option[(String, PackageDef)]
  }

  // ----- Parents --------------------------------------------------

  type Parent /* Term | TypeTree */

  // ----- Terms ----------------------------------------------------

  type Term <: Statement with Parent

  trait TermAPI extends Typed with Positioned {
    def toExpr[T: quoted.Type](implicit ctx: Context): quoted.Expr[T]
  }
  implicit def TermDeco(term: Term): TermAPI

  implicit def termClassTag: ClassTag[Term]

  /** Scala term. Any tree that can go in expression position. */
  val Term: TermModule
  abstract class TermModule {

    /** Matches any term */
    def unapply(x: Term)(implicit ctx: Context): Boolean

    /** Scala term identifier */
    val Ident: IdentExtractor
    abstract class IdentExtractor {
      /** Matches a term identifier and returns its name */
      def unapply(x: Term)(implicit ctx: Context): Option[String]
    }

    /** Scala term selection */
    val Select: SelectExtractor
    abstract class SelectExtractor {
      /** Matches `<qual: Term>.<name: String>: <sig: Signature>` */
      def unapply(x: Term)(implicit ctx: Context): Option[(Term, String, Option[Signature])]
    }

    /** Scala literal constant */
    val Literal: LiteralExtractor
    abstract class LiteralExtractor {
      def unapply(x: Term)(implicit ctx: Context): Option[Constant]
    }

    /** Scala `this` or `this[id]` */
    val This: ThisExtractor
    abstract class ThisExtractor {
      /** Matches new `this[<id: Option[Id]>` */
      def unapply(x: Term)(implicit ctx: Context): Option[Option[Id]]
    }

    /** Scala `new` */
    val New: NewExtractor
    abstract class NewExtractor {
      /** Matches new `new <tpt: TypeTree>` */
      def unapply(x: Term)(implicit ctx: Context): Option[TypeTree]
    }

    /** Scala named argument `x = y` in argument position */
    val NamedArg: NamedArgExtractor
    abstract class NamedArgExtractor {
      /** Matches `<name: String> = <value: Term>` */
      def unapply(x: Term)(implicit ctx: Context): Option[(String, Term)]
    }

    /** Scala parameter application */
    val Apply: ApplyExtractor
    abstract class ApplyExtractor {
      /** Matches function application `<fun: Term>(<args: List[Term]>)` */
      def unapply(x: Term)(implicit ctx: Context): Option[(Term, List[Term])]
    }

    /** Scala type parameter application */
    val TypeApply: TypeApplyExtractor
    abstract class TypeApplyExtractor {
      /** Matches function type application `<fun: Term>[<args: List[TypeTree]>]` */
      def unapply(x: Term)(implicit ctx: Context): Option[(Term, List[TypeTree])]
    }

    /** Scala `x.super` or `x.super[id]` */
    val Super: SuperExtractor
    abstract class SuperExtractor {
      /** Matches new `<qualifier: Term>.super[<id: Option[Id]>` */
      def unapply(x: Term)(implicit ctx: Context): Option[(Term, Option[Id])]
    }

    /** Scala ascription `x: T` */
    val Typed: TypedExtractor
    abstract class TypedExtractor {
      /** Matches `<x: Term>: <tpt: Term>` */
      def unapply(x: Term)(implicit ctx: Context): Option[(Term, TypeTree)]
    }

    /** Scala assign `x = y` */
    val Assign: AssignExtractor
    abstract class AssignExtractor {
      /** Matches `<lhs: Term> = <rhs: Term>` */
      def unapply(x: Term)(implicit ctx: Context): Option[(Term, Term)]
    }

    /** Scala code block `{ stat0; ...; statN; expr }` term */
    val Block: BlockExtractor
    abstract class BlockExtractor {
      /** Matches `{ <statements: List[Statement]>; <expr: Term> }` */
      def unapply(x: Term)(implicit ctx: Context): Option[(List[Statement], Term)]
    }

    val Lambda: LambdaExtractor
    abstract class LambdaExtractor {
      def unapply(x: Term)(implicit ctx: Context): Option[(Term, Option[TypeTree])]
    }

    /** Scala `if`/`else` term */
    val If: IfExtractor
    abstract class IfExtractor {
      /** Matches `if (<cond: Term>) <thenp: Term> else <elsep: Term>` */
      def unapply(x: Term)(implicit ctx: Context): Option[(Term, Term, Term)]
    }

    /** Scala `match` term */
    val Match: MatchExtractor
    abstract class MatchExtractor {
      /** Matches `<scrutinee: Trem> match { <cases: List[CaseDef]> }` */
      def unapply(x: Term)(implicit ctx: Context): Option[(Term, List[CaseDef])]
    }

    /** Scala `try`/`catch`/`finally` term */
    val Try: TryExtractor
    abstract class TryExtractor {
      /** Matches `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>` */
      def unapply(x: Term)(implicit ctx: Context): Option[(Term, List[CaseDef], Option[Term])]
    }

    /** Scala local `return` */
    val Return: ReturnExtractor
    abstract class ReturnExtractor {
      /** Matches `return <expr: Term>` */
      def unapply(x: Term)(implicit ctx: Context): Option[Term]
    }

    val Repeated: RepeatedExtractor
    abstract class RepeatedExtractor {
      def unapply(x: Term)(implicit ctx: Context): Option[List[Term]]
    }

    val Inlined: InlinedExtractor
    abstract class InlinedExtractor {
      def unapply(x: Term)(implicit ctx: Context): Option[(Option[Term], List[Definition], Term)]
    }

    val SelectOuter: SelectOuterExtractor
    abstract class SelectOuterExtractor {
      def unapply(x: Term)(implicit ctx: Context): Option[(Term, Int, Type)]
    }

    val While: WhileExtractor
    abstract class WhileExtractor {
      /** Extractor for while loops. Matches `while (<cond>) <body>` and returns (<cond>, <body>) */
      def unapply(x: Term)(implicit ctx: Context): Option[(Term, Term)]
    }

    val DoWhile: DoWhileExtractor
    abstract class DoWhileExtractor {
      /** Extractor for do while loops. Matches `do <body> while (<cond>)` and returns (<body>, <cond>) */
      def unapply(x: Term)(implicit ctx: Context): Option[(Term, Term)]
    }
  }

  // ----- CaseDef --------------------------------------------------

  type CaseDef

  implicit def caseDefClassTag: ClassTag[CaseDef]

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

  implicit def patternClassTag: ClassTag[Pattern]

  val Pattern: PatternModule
  abstract class PatternModule {

    val Value: ValueExtractor
    abstract class ValueExtractor {
      def unapply(x: Pattern)(implicit ctx: Context): Option[Term]
    }

    val Bind: BindExtractor
    abstract class BindExtractor {
      def unapply(x: Pattern)(implicit ctx: Context): Option[(String, Pattern)]
    }

    val Unapply: UnapplyExtractor
    abstract class UnapplyExtractor {
      def unapply(x: Pattern)(implicit ctx: Context): Option[(Term, List[Term], List[Pattern])]
    }

    val Alternative: AlternativeExtractor
    abstract class AlternativeExtractor {
      def unapply(x: Pattern)(implicit ctx: Context): Option[List[Pattern]]
    }

    val TypeTest: TypeTestExtractor
    abstract class TypeTestExtractor {
      def unapply(x: Pattern)(implicit ctx: Context): Option[TypeTree]
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

  type TypeTree <: TypeOrBoundsTree with Parent

  trait TypeTreeAPI extends Typed with Positioned
  implicit def TypeTreeDeco(tpt: TypeTree): TypeTreeAPI

  implicit def typeTreeClassTag: ClassTag[TypeTree]

  val TypeTree: TypeTreeModule
  abstract class TypeTreeModule {

    def unapply(x: TypeTree)(implicit ctx: Context): Boolean

    /** TypeTree containing an inferred type */
    val Synthetic: SyntheticExtractor
    abstract class SyntheticExtractor {
      /** Matches a TypeTree containing an inferred type */
      def unapply(x: TypeTree)(implicit ctx: Context): Boolean
    }

    val TypeIdent: TypeIdentExtractor
    abstract class TypeIdentExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[String]
    }

    val TermSelect: TermSelectExtractor
    abstract class TermSelectExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(Term, String)]
    }

    val TypeSelect: TypeSelectExtractor
    abstract class TypeSelectExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(TypeTree, String)]
    }

    val Singleton: SingletonExtractor
    abstract class SingletonExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[Term]
    }

    val Refined: RefinedExtractor
    abstract class RefinedExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(TypeTree, List[Definition])]
    }

    val Applied: AppliedExtractor
    abstract class AppliedExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(TypeTree, List[TypeOrBoundsTree])]
    }

    val Annotated: AnnotatedExtractor
    abstract class AnnotatedExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(TypeTree, Term)]
    }

    val And: AndExtractor
    abstract class AndExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(TypeTree, TypeTree)]
    }

    val Or: OrExtractor
    abstract class OrExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(TypeTree, TypeTree)]
    }

    val ByName: ByNameExtractor
    abstract class ByNameExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[TypeTree]
    }

    val TypeLambdaTree: TypeLambdaTreeExtractor
    abstract class TypeLambdaTreeExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(List[TypeDef], TypeOrBoundsTree)]
    }

    val Bind: BindExtractor
    abstract class BindExtractor{
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(String, TypeBoundsTree)]
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

  implicit def typeBoundsTreeClassTag: ClassTag[TypeBoundsTree]

  val TypeBoundsTree: TypeBoundsTreeExtractor
  abstract class TypeBoundsTreeExtractor {
    def unapply(x: TypeBoundsTree)(implicit ctx: Context): Option[(TypeTree, TypeTree)]
  }

  /** TypeBoundsTree containing an inferred type bounds */
  val SyntheticBounds: SyntheticBoundsExtractor
  abstract class SyntheticBoundsExtractor {
    /** Matches a TypeBoundsTree containing inferred type bounds */
    def unapply(x: TypeBoundsTree)(implicit ctx: Context): Boolean
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

  implicit def typeClassTag: ClassTag[Type]
  implicit def methodTypeClassTag: ClassTag[MethodType]
  implicit def polyTypeClassTag: ClassTag[PolyType]
  implicit def typeLambdaClassTag: ClassTag[TypeLambda]
  implicit def recursiveTypeClassTag: ClassTag[RecursiveType]

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

  val Type: TypeModule
  abstract class TypeModule {

    def unapply(x: Type)(implicit ctx: Context): Boolean

    val ConstantType: ConstantTypeExtractor
    abstract class ConstantTypeExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[Constant]
    }

    val SymRef: SymRefExtractor
    abstract class SymRefExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[(Definition, TypeOrBounds /* Type | NoPrefix */)]
    }

    val TermRef: TermRefExtractor
    abstract class TermRefExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[(String, TypeOrBounds /* Type | NoPrefix */)]
    }

    val TypeRef: TypeRefExtractor
    abstract class TypeRefExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[(String, TypeOrBounds /* Type | NoPrefix */)]
    }

    val SuperType: SuperTypeExtractor
    abstract class SuperTypeExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[(Type, Type)]
    }

    val Refinement: RefinementExtractor
    abstract class RefinementExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[(Type, String, TypeOrBounds /* Type | TypeBounds */)]
    }

    val AppliedType: AppliedTypeExtractor
    abstract class AppliedTypeExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[(Type, List[TypeOrBounds /* Type | TypeBounds */])]
    }

    val AnnotatedType: AnnotatedTypeExtractor
    abstract class AnnotatedTypeExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[(Type, Term)]
    }

    val TypeOf: TypeOfExtractor
    abstract class TypeOfExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[(Type, Term)]
    }

    val AndType: AndTypeExtractor
    abstract class AndTypeExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[(Type, Type)]
    }

    val OrType: OrTypeExtractor
    abstract class OrTypeExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[(Type, Type)]
    }

    val ByNameType: ByNameTypeExtractor
    abstract class ByNameTypeExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[Type]
    }

    val ParamRef: ParamRefExtractor
    abstract class ParamRefExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[(LambdaType[TypeOrBounds], Int)]
    }

    val ThisType: ThisTypeExtractor
    abstract class ThisTypeExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[Type]
    }

    val RecursiveThis: RecursiveThisExtractor
    abstract class RecursiveThisExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[RecursiveType]
    }

    val RecursiveType: RecursiveTypeExtractor
    abstract class RecursiveTypeExtractor {
      def unapply(x: RecursiveType)(implicit ctx: Context): Option[Type]
    }

    val MethodType: MethodTypeExtractor
    abstract class MethodTypeExtractor {
      def unapply(x: MethodType)(implicit ctx: Context): Option[(List[String], List[Type], Type)]
    }

    val PolyType: PolyTypeExtractor
    abstract class PolyTypeExtractor {
      def unapply(x: PolyType)(implicit ctx: Context): Option[(List[String], List[TypeBounds], Type)]
    }

    val TypeLambda: TypeLambdaExtractor
    abstract class TypeLambdaExtractor {
      def unapply(x: TypeLambda)(implicit ctx: Context): Option[(List[String], List[TypeBounds], Type)]
    }

  }

  // ----- TypeBounds -----------------------------------------------

  type TypeBounds <: TypeOrBounds

  implicit def typeBoundsClassTag: ClassTag[TypeBounds]

  val TypeBounds: TypeBoundsExtractor
  abstract class TypeBoundsExtractor {
    def unapply(x: TypeBounds)(implicit ctx: Context): Option[(Type, Type)]
  }

  trait TypeBoundsAPI {
    def low(implicit ctx: Context): Type
    def hi(implicit ctx: Context): Type
  }
  implicit def TypeBoundsDeco(bounds: TypeBounds): TypeBoundsAPI

  // ----- NoPrefix -------------------------------------------------

  type NoPrefix <: TypeOrBounds

  implicit def noPrefixClassTag: ClassTag[NoPrefix]

  val NoPrefix: NoPrefixExtractor
  abstract class NoPrefixExtractor {
    def unapply(x: NoPrefix)(implicit ctx: Context): Boolean
  }

  // ===== Constants ================================================

  type Constant
  trait ConstantAPI {
    def show(implicit ctx: Context, s: Show[tasty.type]): String
    def value: Any
  }
  implicit def ConstantDeco(const: Constant): ConstantAPI

  implicit def constantClassTag: ClassTag[Constant]

  val Constant: ConstantModule
  abstract class ConstantModule {

    val Unit: UnitExtractor
    abstract class UnitExtractor {
      def unapply(x: Constant): Boolean
    }

    val Null: NullExtractor
    abstract class NullExtractor {
      def unapply(x: Constant): Boolean
    }

    val Boolean: BooleanExtractor
    abstract class BooleanExtractor {
      def unapply(x: Constant): Option[Boolean]
    }

    val Byte: ByteExtractor
    abstract class ByteExtractor {
      def unapply(x: Constant): Option[Byte]
    }

    val Short: ShortExtractor
    abstract class ShortExtractor {
      def unapply(x: Constant): Option[Short]
    }

    val Char: CharExtractor
    abstract class CharExtractor {
      def unapply(x: Constant): Option[Char]
    }

    val Int: IntExtractor
    abstract class IntExtractor {
      def unapply(x: Constant): Option[Int]
    }

    val Long: LongExtractor
    abstract class LongExtractor {
      def unapply(x: Constant): Option[Long]
    }

    val Float: FloatExtractor
    abstract class FloatExtractor {
      def unapply(x: Constant): Option[Float]
    }

    val Double: DoubleExtractor
    abstract class DoubleExtractor {
      def unapply(x: Constant): Option[Double]
    }

    val String: StringExtractor
    abstract class StringExtractor {
      def unapply(x: Constant): Option[String]
    }

    val ClassTag: ClassTagExtractor
    abstract class ClassTagExtractor {
      def unapply(x: Constant): Option[Type]
    }

    /** Extractor for scala.Symbol literals */
    val Symbol: SymbolExtractor
    /** Extractor for scala.Symbol literals */
    abstract class SymbolExtractor {
      def unapply(x: Constant): Option[scala.Symbol]
    }
  }

  // ===== Signature ================================================

  type Signature

  implicit def signatureClassTag: ClassTag[Signature]

  val Signature: SignatureExtractor
  abstract class SignatureExtractor {
    def unapply(x: Signature)(implicit ctx: Context): Option[(List[String], String)]
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
