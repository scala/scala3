package scala.tasty

import scala.reflect.ClassTag

abstract class Tasty {

  // ===== Quotes ===================================================

  trait AbstractQuotedExpr {
    def toTasty(implicit ctx: Context): Term
  }
  implicit def QuotedExprDeco[T](x: quoted.Expr[T]): AbstractQuotedExpr

  trait AbstractQuotedType {
    def toTasty(implicit ctx: Context): TypeTree
  }
  implicit def QuotedTypeDeco[T](x: quoted.Type[T]): AbstractQuotedType

  // ===== Contexts =================================================

  type Context

  trait AbstractContext {
    def owner: Definition
  }
  implicit def ContextDeco(x: Context): AbstractContext

  // ===== Id =======================================================

  type Id

  trait AbstractId extends Positioned
  implicit def IdDeco(x: Id): AbstractId

  implicit def idClassTag: ClassTag[Id]

  val Id: IdExtractor
  abstract class IdExtractor {
    def unapply(x: Id): Option[String]
  }

  // ===== Trees ====================================================

  type Tree

  trait AbstractTree extends Positioned
  implicit def TreeDeco(t: Tree): AbstractTree

  type PackageClause <: Tree

  implicit def packageClauseClassTag: ClassTag[PackageClause]

  val PackageClause: PackageClauseExtractor
  abstract class PackageClauseExtractor {
    def unapply(x: PackageClause)(implicit ctx: Context): Option[(Term, List[Tree])]
  }

  trait AbstractPackageClause {
    def definition: Definition
  }
  implicit def PackageClauseDeco(x: PackageClause): AbstractPackageClause

  // ----- Statements -----------------------------------------------

  type Statement <: Tree

  type Import <: Statement

  implicit def importClassTag: ClassTag[Import]

  val Import: ImportExtractor
  abstract class ImportExtractor {
    def unapply(x: Import)(implicit ctx: Context): Option[(Term, List[ImportSelector])]
  }

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

  implicit def definitionClassTag: ClassTag[Definition]

  trait AbstractDefinition {
    def mods(implicit ctx: Context): List[Modifier]
    def owner(implicit ctx: Context): Definition
    def localContext(implicit ctx: Context): Context
  }
  implicit def DefinitionDeco(x: Definition): AbstractDefinition

  // ClassDef

  type ClassDef <: Definition

  implicit def classDefClassTag: ClassTag[ClassDef]

  val ClassDef: ClassDefExtractor
  abstract class ClassDefExtractor {
    def unapply(x: ClassDef)(implicit ctx: Context): Option[(String, DefDef, List[Parent], Option[ValDef], List[Statement])]
  }

  // DefDef

  type DefDef <: Definition

  implicit def defDefClassTag: ClassTag[DefDef]

  val DefDef: DefDefExtractor
  abstract class DefDefExtractor {
    def unapply(x: DefDef)(implicit ctx: Context): Option[(String, List[TypeDef],  List[List[ValDef]], TypeTree, Option[Term])]
  }

  // ValDef

  type ValDef <: Definition

  implicit def valDefClassTag: ClassTag[ValDef]

  val ValDef: ValDefExtractor
  abstract class ValDefExtractor {
    def unapply(x: ValDef)(implicit ctx: Context): Option[(String, TypeTree, Option[Term])]
  }

  // TypeDef

  type TypeDef <: Definition

  implicit def typeDefClassTag: ClassTag[TypeDef]

  val TypeDef: TypeDefExtractor
  abstract class TypeDefExtractor {
    def unapply(x: TypeDef)(implicit ctx: Context): Option[(String, TypeOrBoundsTree /* TypeTree | TypeBoundsTree */)]
  }

  // PackageDef

  type PackageDef <: Definition

  implicit def packageDefClassTag: ClassTag[PackageDef]

  val PackageDef: PackageDefExtractor
  abstract class PackageDefExtractor {
    def unapply(x: PackageDef)(implicit ctx: Context): Option[(String, List[Statement])]
  }

  // ----- Parents --------------------------------------------------

  type Parent

  implicit def parentClassTag: ClassTag[Parent]

  val TermParent: TermParentExtractor
  abstract class TermParentExtractor {
    def unapply(x: Parent)(implicit ctx: Context): Option[Term]
  }

  val TypeParent: TypeParentExtractor
  abstract class TypeParentExtractor {
    def unapply(x: Parent)(implicit ctx: Context): Option[TypeTree]
  }

  // ----- Terms ----------------------------------------------------

  type Term <: Statement

  trait AbstractTerm extends Typed with Positioned
  implicit def TermDeco(t: Term): AbstractTerm

  implicit def termClassTag: ClassTag[Term]

  val Ident: IdentExtractor
  abstract class IdentExtractor {
    def unapply(x: Term)(implicit ctx: Context): Option[String]
  }

  val Select: SelectExtractor
  abstract class SelectExtractor {
    def unapply(x: Term)(implicit ctx: Context): Option[(Term, String, Option[Signature])]
  }

  val Literal: LiteralExtractor
  abstract class LiteralExtractor {
    def unapply(x: Term)(implicit ctx: Context): Option[Constant]
  }

  val This: ThisExtractor
  abstract class ThisExtractor {
    def unapply(x: Term)(implicit ctx: Context): Option[Option[Id]]
  }

  val New: NewExtractor
  abstract class NewExtractor {
    def unapply(x: Term)(implicit ctx: Context): Option[TypeTree]
  }

  val NamedArg: NamedArgExtractor
  abstract class NamedArgExtractor {
    def unapply(x: Term)(implicit ctx: Context): Option[(String, Term)]
  }

  val Apply: ApplyExtractor
  abstract class ApplyExtractor {
    def unapply(x: Term)(implicit ctx: Context): Option[(Term, List[Term])]
  }

  val TypeApply: TypeApplyExtractor
  abstract class TypeApplyExtractor {
    def unapply(x: Term)(implicit ctx: Context): Option[(Term, List[TypeTree])]
  }

  val Super: SuperExtractor
  abstract class SuperExtractor {
    def unapply(x: Term)(implicit ctx: Context): Option[(Term, Option[Id])]
  }

  val Typed: TypedExtractor
  abstract class TypedExtractor {
    def unapply(x: Term)(implicit ctx: Context): Option[(Term, TypeTree)]
  }

  val Assign: AssignExtractor
  abstract class AssignExtractor {
    def unapply(x: Term)(implicit ctx: Context): Option[(Term, Term)]
  }

  val Block: BlockExtractor
  abstract class BlockExtractor {
    def unapply(x: Term)(implicit ctx: Context): Option[(List[Statement], Term)]
  }

  val Inlined: InlinedExtractor
  abstract class InlinedExtractor {
    def unapply(x: Term)(implicit ctx: Context): Option[(Term, List[Definition], Term)]
  }

  val Lambda: LambdaExtractor
  abstract class LambdaExtractor {
    def unapply(x: Term)(implicit ctx: Context): Option[(Term, Option[TypeTree])]
  }

  val If: IfExtractor
  abstract class IfExtractor {
    def unapply(x: Term)(implicit ctx: Context): Option[(Term, Term, Term)]
  }

  val Match: MatchExtractor
  abstract class MatchExtractor {
    def unapply(x: Term)(implicit ctx: Context): Option[(Term, List[CaseDef])]
  }

  val Try: TryExtractor
  abstract class TryExtractor {
    def unapply(x: Term)(implicit ctx: Context): Option[(Term, List[CaseDef], Option[Term])]
  }

  val Return: ReturnExtractor
  abstract class ReturnExtractor {
    def unapply(x: Term)(implicit ctx: Context): Option[Term]
  }

  val Repeated: RepeatedExtractor
  abstract class RepeatedExtractor {
    def unapply(x: Term)(implicit ctx: Context): Option[List[Term]]
  }

  val SelectOuter: SelectOuterExtractor
  abstract class SelectOuterExtractor {
    def unapply(x: Term)(implicit ctx: Context): Option[(Term, Int, Type)]
  }

  // ----- CaseDef --------------------------------------------------

  type CaseDef

  implicit def caseDefClassTag: ClassTag[CaseDef]

  val CaseDef: CaseDefExtractor
  abstract class CaseDefExtractor {
    def unapply(x: CaseDef): Option[(Pattern, Option[Term], Term)]
  }

  // ----- Patterns -------------------------------------------------

  type Pattern

  trait AbstractPattern extends Typed with Positioned
  implicit def PatternDeco(x: Pattern): AbstractPattern

  implicit def patternClassTag: ClassTag[Pattern]

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

  // ----- TypeTrees ------------------------------------------------

  type TypeOrBoundsTree

  trait AbstractTypeOrBoundsTree {
    def tpe(implicit ctx: Context): TypeOrBounds
  }
  implicit def TypeOrBoundsTreeDeco(x: TypeOrBoundsTree): AbstractTypeOrBoundsTree


  // ----- TypeTrees ------------------------------------------------

  type TypeTree <: TypeOrBoundsTree

  trait AbstractTypeTree extends Typed with Positioned
  implicit def TypeTreeDeco(x: TypeTree): AbstractTypeTree

  implicit def typeTreeClassTag: ClassTag[TypeTree]

  val Synthetic: SyntheticExtractor
  abstract class SyntheticExtractor {
    def unapply(x: TypeTree)(implicit ctx: Context): Boolean
  }

  val TypeIdent: TypeIdentExtractor
  abstract class TypeIdentExtractor {
    def unapply(x: TypeTree)(implicit ctx: Context): Option[String]
  }

  val TypeSelect: TypeSelectExtractor
  abstract class TypeSelectExtractor {
    def unapply(x: TypeTree)(implicit ctx: Context): Option[(Term, String)]
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
    def unapply(x: TypeTree)(implicit ctx: Context): Option[(TypeTree, List[TypeTree])]
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

  // ----- TypeBoundsTrees ------------------------------------------------

  type TypeBoundsTree <: TypeOrBoundsTree

  trait AbstractTypeBoundsTree {
    def tpe: TypeBounds
  }
  implicit def TypeBoundsTreeDeco(x: TypeBoundsTree): AbstractTypeBoundsTree

  implicit def typeBoundsTreeClassTag: ClassTag[TypeBoundsTree]

  val TypeBoundsTree: TypeBoundsTreeExtractor
  abstract class TypeBoundsTreeExtractor {
    def unapply(x: TypeBoundsTree)(implicit ctx: Context): Option[(TypeTree, TypeTree)]
  }

  // ===== Types ====================================================

  type TypeOrBounds

  trait Typed {
    def tpe(implicit ctx: Context): Type
  }

  // ----- Types ----------------------------------------------------

  type Type <: TypeOrBounds

  implicit def typeClassTag: ClassTag[Type]

  val ConstantType: ConstantTypeExtractor
  abstract class ConstantTypeExtractor {
    def unapply(x: Type)(implicit ctx: Context): Option[Constant]
  }

  val SymRef: SymRefExtractor
  abstract class SymRefExtractor {
    def unapply(x: Type)(implicit ctx: Context): Option[(Definition, TypeOrBounds /* Type | NoPrefix */)]
  }

  val NameRef: NameRefExtractor
  abstract class NameRefExtractor {
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

  type RecursiveType <: Type
  implicit def recursiveTypeClassTag: ClassTag[RecursiveType]
  val RecursiveType: RecursiveTypeExtractor
  abstract class RecursiveTypeExtractor {
    def unapply(x: RecursiveType)(implicit ctx: Context): Option[Type]
  }

  // ----- Methodic Types -------------------------------------------

  type LambdaType[ParamInfo <: TypeOrBounds] <: Type

  type MethodType <: LambdaType[Type]

  trait AbstractMethodType {
    def isImplicit: Boolean
    def isErased: Boolean
  }
  implicit def MethodTypeDeco(x: MethodType): AbstractMethodType

  implicit def methodTypeClassTag: ClassTag[MethodType]

  val MethodType: MethodTypeExtractor
  abstract class MethodTypeExtractor {
    def unapply(x: MethodType)(implicit ctx: Context): Option[(List[String], List[Type], Type)]
  }

  type PolyType <: LambdaType[TypeBounds]

  implicit def polyTypeClassTag: ClassTag[PolyType]

  val PolyType: PolyTypeExtractor
  abstract class PolyTypeExtractor {
    def unapply(x: PolyType)(implicit ctx: Context): Option[(List[String], List[TypeBounds], Type)]
  }

  type TypeLambda <: LambdaType[TypeBounds]

  implicit def typeLambdaClassTag: ClassTag[TypeLambda]

  val TypeLambda: TypeLambdaExtractor
  abstract class TypeLambdaExtractor {
    def unapply(x: TypeLambda)(implicit ctx: Context): Option[(List[String], List[TypeBounds], Type)]
  }

  // ----- TypeBounds -----------------------------------------------

  type TypeBounds <: TypeOrBounds

  implicit def typeBoundsClassTag: ClassTag[TypeBounds]

  val TypeBounds: TypeBoundsExtractor
  abstract class TypeBoundsExtractor {
    def unapply(x: TypeBounds)(implicit ctx: Context): Option[(Type, Type)]
  }

  // ----- NoPrefix -------------------------------------------------

  type NoPrefix <: TypeOrBounds

  implicit def noPrefixClassTag: ClassTag[NoPrefix]

  val NoPrefix: NoPrefixExtractor
  abstract class NoPrefixExtractor {
    def unapply(x: NoPrefix)(implicit ctx: Context): Boolean
  }

  // ===== Constants ================================================

  type Constant
  trait AbstractConstant {
    def value: Any
  }
  implicit def ConstantDeco(x: Constant): AbstractConstant

  implicit def constantClassTag: ClassTag[Constant]

  val UnitConstant: UnitExtractor
  abstract class UnitExtractor {
    def unapply(x: Constant): Boolean
  }

  val NullConstant: NullExtractor
  abstract class NullExtractor {
    def unapply(x: Constant): Boolean
  }

  val BooleanConstant: BooleanExtractor
  abstract class BooleanExtractor {
    def unapply(x: Constant): Option[Boolean]
  }

  val ByteConstant: ByteExtractor
  abstract class ByteExtractor {
    def unapply(x: Constant): Option[Byte]
  }

  val ShortConstant: ShortExtractor
  abstract class ShortExtractor {
    def unapply(x: Constant): Option[Short]
  }

  val CharConstant: CharExtractor
  abstract class CharExtractor {
    def unapply(x: Constant): Option[Char]
  }

  val IntConstant: IntExtractor
  abstract class IntExtractor {
    def unapply(x: Constant): Option[Int]
  }

  val LongConstant: LongExtractor
  abstract class LongExtractor {
    def unapply(x: Constant): Option[Long]
  }

  val FloatConstant: FloatExtractor
  abstract class FloatExtractor {
    def unapply(x: Constant): Option[Float]
  }

  val DoubleConstant: DoubleExtractor
  abstract class DoubleExtractor {
    def unapply(x: Constant): Option[Double]
  }

  val StringConstant: StringExtractor
  abstract class StringExtractor {
    def unapply(x: Constant): Option[String]
  }

  // ===== Modifiers ================================================

  type Modifier

  implicit def modifierClassTag: ClassTag[Modifier]

  val Annotation: AnnotationExtractor
  abstract class AnnotationExtractor {
    def unapply(x: Modifier)(implicit ctx: Context): Option[Term]
  }

  val Flags: FlagsExtractor
  abstract class FlagsExtractor {
    def unapply(x: Modifier)(implicit ctx: Context): Option[FlagSet]
  }

  val QualifiedPrivate: QualifiedPrivateExtractor
  abstract class QualifiedPrivateExtractor {
    def unapply(x: Modifier)(implicit ctx: Context): Option[Type]
  }

  val QualifiedProtected: QualifiedProtectedExtractor
  abstract class QualifiedProtectedExtractor {
    def unapply(x: Modifier)(implicit ctx: Context): Option[Type]
  }

  // ===== Signature ================================================

  type Signature

  implicit def signatureClassTag: ClassTag[Signature]

  val Signature: SignatureExtractor
  abstract class SignatureExtractor {
    def unapply(x: Signature)(implicit ctx: Context): Option[(List[String], String)]
  }

  // ===== Positions ================================================

  trait Position {
    def start: Int
    def end: Int

    def sourceFile: java.nio.file.Path

    def startLine: Int
    def startColumn: Int
    def endLine: Int
    def endColumn: Int
  }

  trait Positioned {
    def pos(implicit ctx: Context): Position
  }

}
