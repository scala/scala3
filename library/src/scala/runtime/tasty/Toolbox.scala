package scala.runtime.tasty

import scala.tasty.constants.Constant
import scala.tasty.modifiers._
import scala.tasty.names._
import scala.tasty.trees._
import scala.tasty.types._
import scala.tasty.Context

trait Toolbox {

  // Statements

  def unapplyPackageClause(arg: PackageClause)(implicit ctx: Context): Option[PackageClause.Data]
  def unapplyImport(arg: Import)(implicit ctx: Context): Option[Import.Data]

  // Definitions

  def unapplyValDef(arg: ValDef)(implicit ctx: Context): Option[ValDef.Data]
  def unapplyDefDef(arg: DefDef)(implicit ctx: Context): Option[DefDef.Data]
  def unapplyTypeDef(arg: TypeDef)(implicit ctx: Context): Option[TypeDef.Data]
  def unapplyClassDef(arg: ClassDef)(implicit ctx: Context): Option[ClassDef.Data]
  def unapplyPackageDef(arg: PackageDef)(implicit ctx: Context): Option[PackageDef.Data]

  // Terms

  def unapplyIdent(arg: Term)(implicit ctx: Context): Option[Ident.Data]
  def unapplySelect(arg: Term)(implicit ctx: Context): Option[Select.Data]
  def unapplyLiteral(arg: Term)(implicit ctx: Context): Option[Literal.Data]
  def unapplyThis(arg: Term)(implicit ctx: Context): Option[This.Data]
  def unapplyNew(arg: Term)(implicit ctx: Context): Option[New.Data]
  def unapplyNamedArg(arg: Term)(implicit ctx: Context): Option[NamedArg.Data]
  def unapplyApply(arg: Term)(implicit ctx: Context): Option[Apply.Data]
  def unapplyTypeApply(arg: Term)(implicit ctx: Context): Option[TypeApply.Data]
  def unapplySuper(arg: Term)(implicit ctx: Context): Option[Super.Data]
  def unapplyTyped(arg: Term)(implicit ctx: Context): Option[Typed.Data]
  def unapplyAssign(arg: Term)(implicit ctx: Context): Option[Assign.Data]
  def unapplyBlock(arg: Term)(implicit ctx: Context): Option[Block.Data]
  def unapplyInlined(arg: Term)(implicit ctx: Context): Option[Inlined.Data]
  def unapplyLambda(arg: Term)(implicit ctx: Context): Option[Lambda.Data]
  def unapplyIf(arg: Term)(implicit ctx: Context): Option[If.Data]
  def unapplyMatch(arg: Term)(implicit ctx: Context): Option[Match.Data]
  def unapplyTry(arg: Term)(implicit ctx: Context): Option[Try.Data]
  def unapplyReturn(arg: Term)(implicit ctx: Context): Option[Return.Data]
  def unapplyRepeated(arg: Term)(implicit ctx: Context): Option[Repeated.Data]
  def unapplySelectOuter(arg: Term)(implicit ctx: Context): Option[SelectOuter.Data]

  // Patterns

  def unapplyCaseDef(arg: CaseDef)(implicit ctx: Context): Option[CaseDef.Data]

  def unapplyValue(arg: Pattern)(implicit ctx: Context): Option[Value.Data]
  def unapplyBind(arg: Pattern)(implicit ctx: Context): Option[Bind.Data]
  def unapplyUnapply(arg: Pattern)(implicit ctx: Context): Option[Unapply.Data]
  def unapplyAlternative(arg: Pattern)(implicit ctx: Context): Option[Alternative.Data]
  def unapplyTypeTest(arg: Pattern)(implicit ctx: Context): Option[TypeTest.Data]

  // Type trees

  def unapplySynthetic(arg: TypeTree)(implicit ctx: Context): Boolean
  def unapplyTypeIdent(arg: TypeTree)(implicit ctx: Context): Option[TypeIdent.Data]
  def unapplyTypeSelect(arg: TypeTree)(implicit ctx: Context): Option[TypeSelect.Data]
  def unapplySingleton(arg: TypeTree)(implicit ctx: Context): Option[Singleton.Data]
  def unapplyRefined(arg: TypeTree)(implicit ctx: Context): Option[Refined.Data]
  def unapplyApplied(arg: TypeTree)(implicit ctx: Context): Option[Applied.Data]
  def unapplyAnnotated(arg: TypeTree)(implicit ctx: Context): Option[Annotated.Data]
  def unapplyAnd(arg: TypeTree)(implicit ctx: Context): Option[And.Data]
  def unapplyOr(arg: TypeTree)(implicit ctx: Context): Option[Or.Data]
  def unapplyByName(arg: TypeTree)(implicit ctx: Context): Option[ByName.Data]

  def unapplyTypeBoundsTree(arg: TypeBoundsTree)(implicit ctx: Context): Option[TypeBoundsTree.Data]

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

  def unapplyConstantType(arg: Type)(implicit ctx: Context): Option[ConstantType.Data]
  def unapplySymRef(arg: Type)(implicit ctx: Context): Option[SymRef.Data]
  def unapplyNameRef(arg: Type)(implicit ctx: Context): Option[NameRef.Data]
  def unapplySuperType(arg: Type)(implicit ctx: Context): Option[SuperType.Data]
  def unapplyRefinement(arg: Type)(implicit ctx: Context): Option[Refinement.Data]
  def unapplyAppliedType(arg: Type)(implicit ctx: Context): Option[AppliedType.Data]
  def unapplyAnnotatedType(arg: Type)(implicit ctx: Context): Option[AnnotatedType.Data]
  def unapplyAndType(arg: Type)(implicit ctx: Context): Option[AndType.Data]
  def unapplyOrType(arg: Type)(implicit ctx: Context): Option[OrType.Data]
  def unapplyByNameType(arg: Type)(implicit ctx: Context): Option[ByNameType.Data]
  def unapplyParamRef(arg: Type)(implicit ctx: Context): Option[ParamRef.Data]
  def unapplyThisType(arg: Type)(implicit ctx: Context): Option[ThisType.Data]
  def unapplyRecursiveThis(arg: Type)(implicit ctx: Context): Option[RecursiveThis.Data]

  def unapplyRecursiveType(arg: RecursiveType)(implicit ctx: scala.tasty.Context): Option[RecursiveType.Data]

  def unapplyMethodType(arg: MethodType)(implicit ctx: scala.tasty.Context): Option[MethodType.Data]
  def unapplyPolyType(arg: PolyType)(implicit ctx: scala.tasty.Context): Option[PolyType.Data]
  def unapplyTypeLambda(arg: TypeLambda)(implicit ctx: scala.tasty.Context): Option[TypeLambda.Data]

  def unapplyTypeBounds(arg: TypeBounds)(implicit ctx: scala.tasty.Context): Option[TypeBounds.Data]

  // Modifiers

  def unapplyFlags(arg: Flags)(implicit ctx: Context): Option[Flags.Data]
  def unapplyQualifiedPrivate(arg: Qualified)(implicit ctx: Context): Option[QualifiedPrivate.Data]
  def unapplyQualifiedProtected(arg: Qualified)(implicit ctx: Context): Option[QualifiedProtected.Data]
  def unapplyAnnotation(arg: Annotation)(implicit ctx: Context): Option[Annotation.Data]

  // Import selectors

  def unapplySimpleSelector(arg: ImportSelector)(implicit ctx: Context): Option[SimpleSelector.Data]
  def unapplyRenameSelector(arg: ImportSelector)(implicit ctx: Context): Option[RenameSelector.Data]
  def unapplyOmitSelector(arg: ImportSelector)(implicit ctx: Context): Option[OmitSelector.Data]

}
