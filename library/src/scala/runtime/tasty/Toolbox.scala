package scala.runtime.tasty

import scala.annotation.implicitNotFound

import scala.tasty.constants.Constant
import scala.tasty.modifiers._
import scala.tasty.names._
import scala.tasty.trees._
import scala.tasty.types._

@implicitNotFound("Could not find implicit tasty.Toolbox. Default toolbox can be imported with `import dotty.tools.dotc.tasty.Toolbox._`")
trait Toolbox {

  // Statements

  def unapplyPackageClause(arg: PackageClause): Option[PackageClause.Data]
  def unapplyImport(arg: Import): Option[Import.Data]

  // Definitions

  def unapplyValDef(arg: ValDef): Option[ValDef.Data]
  def unapplyDefDef(arg: DefDef): Option[DefDef.Data]
  def unapplyTypeDef(arg: TypeDef): Option[TypeDef.Data]
  def unapplyClassDef(arg: ClassDef): Option[ClassDef.Data]
  def unapplyPackageDef(arg: PackageDef): Option[PackageDef.Data]

  // Terms

  def unapplyIdent(arg: Term): Option[Ident.Data]
  def unapplySelect(arg: Term): Option[Select.Data]
  def unapplyLiteral(arg: Term): Option[Literal.Data]
  def unapplyThis(arg: Term): Option[This.Data]
  def unapplyNew(arg: Term): Option[New.Data]
  def unapplyNamedArg(arg: Term): Option[NamedArg.Data]
  def unapplyApply(arg: Term): Option[Apply.Data]
  def unapplyTypeApply(arg: Term): Option[TypeApply.Data]
  def unapplySuper(arg: Term): Option[Super.Data]
  def unapplyTyped(arg: Term): Option[Typed.Data]
  def unapplyAssign(arg: Term): Option[Assign.Data]
  def unapplyBlock(arg: Term): Option[Block.Data]
  def unapplyInlined(arg: Term): Option[Inlined.Data]
  def unapplyLambda(arg: Term): Option[Lambda.Data]
  def unapplyIf(arg: Term): Option[If.Data]
  def unapplyMatch(arg: Term): Option[Match.Data]
  def unapplyTry(arg: Term): Option[Try.Data]
  def unapplyReturn(arg: Term): Option[Return.Data]
  def unapplyRepeated(arg: Term): Option[Repeated.Data]
  def unapplySelectOuter(arg: Term): Option[SelectOuter.Data]

  // Patterns

  def unapplyCaseDef(arg: CaseDef): Option[CaseDef.Data]

  def unapplyValue(arg: Pattern): Option[Value.Data]
  def unapplyBind(arg: Pattern): Option[Bind.Data]
  def unapplyUnapply(arg: Pattern): Option[Unapply.Data]
  def unapplyAlternative(arg: Pattern): Option[Alternative.Data]
  def unapplyTypeTest(arg: Pattern): Option[TypeTest.Data]

  // Type trees

  def unapplySynthetic(arg: TypeTree): Boolean
  def unapplyTypeIdent(arg: TypeTree): Option[TypeIdent.Data]
  def unapplyTypeSelect(arg: TypeTree): Option[TypeSelect.Data]
  def unapplySingleton(arg: TypeTree): Option[Singleton.Data]
  def unapplyRefined(arg: TypeTree): Option[Refined.Data]
  def unapplyApplied(arg: TypeTree): Option[Applied.Data]
  def unapplyAnnotated(arg: TypeTree): Option[Annotated.Data]
  def unapplyAnd(arg: TypeTree): Option[And.Data]
  def unapplyOr(arg: TypeTree): Option[Or.Data]
  def unapplyByName(arg: TypeTree): Option[ByName.Data]

  def unapplyTypeBoundsTree(arg: TypeBoundsTree): Option[TypeBoundsTree.Data]

  // Names

  def unapplySimple(arg: TermName): Option[Simple.Data]
  def unapplyQualified(arg: TermName): Option[Qualified.Data]

  def unapplyDefaultGetter(arg: TermName): Option[DefaultGetter.Data]
  def unapplyVariant(arg: TermName): Option[Variant.Data]
  def unapplySuperAccessor(arg: TermName): Option[SuperAccessor.Data]
  def unapplyProtectedAccessor(arg: TermName): Option[ProtectedAccessor.Data]
  def unapplyProtectedSetter(arg: TermName): Option[ProtectedSetter.Data]
  def unapplyObjectClass(arg: TermName): Option[ObjectClass.Data]

  def unapplySignedName(arg: SignedName): Option[SignedName.Data]

  def unapplyTypeName(arg: TypeName): Option[TypeName.Data]

  // Constants

  def unapplyUnit(arg: Constant): Boolean
  def unapplyNull(arg: Constant): Boolean
  def unapplyBoolean(arg: Constant): Option[Boolean]
  def unapplyByte(arg: Constant): Option[Byte]
  def unapplyChar(arg: Constant): Option[Char]
  def unapplyShort(arg: Constant): Option[Short]
  def unapplyInt(arg: Constant): Option[Int]
  def unapplyLong(arg: Constant): Option[Long]
  def unapplyFloat(arg: Constant): Option[Float]
  def unapplyDouble(arg: Constant): Option[Double]
  def unapplyString(arg: Constant): Option[String]

  // Types

  def unapplyConstantType(arg: Type): Option[ConstantType.Data]
  def unapplySymRef(arg: Type): Option[SymRef.Data]
  def unapplyNameRef(arg: Type): Option[NameRef.Data]
  def unapplySuperType(arg: Type): Option[SuperType.Data]
  def unapplyRefinement(arg: Type): Option[Refinement.Data]
  def unapplyAppliedType(arg: Type): Option[AppliedType.Data]
  def unapplyAnnotatedType(arg: Type): Option[AnnotatedType.Data]
  def unapplyAndType(arg: Type): Option[AndType.Data]
  def unapplyOrType(arg: Type): Option[OrType.Data]
  def unapplyByNameType(arg: Type): Option[ByNameType.Data]
  def unapplyParamRef(arg: Type): Option[ParamRef.Data]
  def unapplyThisType(arg: Type): Option[ThisType.Data]
  def unapplyRecursiveThis(arg: Type): Option[RecursiveThis.Data]

  def unapplyRecursiveType(arg: RecursiveType): Option[RecursiveType.Data]

  def unapplyMethodType(arg: MethodType): Option[MethodType.Data]
  def unapplyPolyType(arg: PolyType): Option[PolyType.Data]
  def unapplyTypeLambda(arg: TypeLambda): Option[TypeLambda.Data]

  def unapplyTypeBounds(arg: TypeBounds): Option[TypeBounds.Data]

  // Modifiers

  def unapplyFlags(arg: Modifier): Option[Flags.Data]
  def unapplyQualifiedPrivate(arg: Modifier): Option[QualifiedPrivate.Data]
  def unapplyQualifiedProtected(arg: Modifier): Option[QualifiedProtected.Data]
  def unapplyAnnotation(arg: Modifier): Option[Annotation.Data]

  // Import selectors

  def unapplySimpleSelector(arg: ImportSelector): Option[SimpleSelector.Data]
  def unapplyRenameSelector(arg: ImportSelector): Option[RenameSelector.Data]
  def unapplyOmitSelector(arg: ImportSelector): Option[OmitSelector.Data]

}
