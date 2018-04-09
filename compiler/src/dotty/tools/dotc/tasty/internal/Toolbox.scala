package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.core.Contexts.Context

import scala.reflect.ClassTag

import scala.tasty.{constants, names, modifiers, trees, types}

object Toolbox extends scala.runtime.tasty.Toolbox {

  // Statements

  override def unapplyPackageClause(arg: trees.PackageClause)(implicit ctx: scala.tasty.Context) = PackageClause.unapplyPackageClause(impl(arg))(ictx)

  override def unapplyImport(arg: trees.Import)(implicit ctx: scala.tasty.Context) = Import.unapplyImport(impl(arg))(ictx)

  override def unapplyValDef(arg: trees.ValDef)(implicit ctx: scala.tasty.Context) = ValDef.unapplyValDef(impl(arg))(ictx)

  override def unapplyDefDef(arg: trees.DefDef)(implicit ctx: scala.tasty.Context) = DefDef.unapplyDefDef(impl(arg))(ictx)

  override def unapplyTypeDef(arg: trees.TypeDef)(implicit ctx: scala.tasty.Context) = TypeDef.unapplyTypeDef(impl(arg))(ictx)

  override def unapplyClassDef(arg: trees.ClassDef)(implicit ctx: scala.tasty.Context) = ClassDef.unapplyClassDef(impl(arg))(ictx)

  override def unapplyPackageDef(arg: trees.PackageDef)(implicit ctx: scala.tasty.Context) = PackageDef.unapplyPackageDef(impl(arg))(ictx)

  // Terms

  override def unapplyIdent(arg: trees.Term)(implicit ctx: scala.tasty.Context) = Term.unapplyIdent(impl(arg))(ictx)

  override def unapplySelect(arg: trees.Term)(implicit ctx: scala.tasty.Context) = Term.unapplySelect(impl(arg))(ictx)

  override def unapplyLiteral(arg: trees.Term)(implicit ctx: scala.tasty.Context) = Term.unapplyLiteral(impl(arg))(ictx)

  override def unapplyThis(arg: trees.Term)(implicit ctx: scala.tasty.Context) = Term.unapplyThis(impl(arg))(ictx)

  override def unapplyNew(arg: trees.Term)(implicit ctx: scala.tasty.Context) = Term.unapplyNew(impl(arg))(ictx)

  override def unapplyNamedArg(arg: trees.Term)(implicit ctx: scala.tasty.Context) = Term.unapplyNamedArg(impl(arg))(ictx)

  override def unapplyApply(arg: trees.Term)(implicit ctx: scala.tasty.Context) = Term.unapplyApply(impl(arg))(ictx)

  override def unapplyTypeApply(arg: trees.Term)(implicit ctx: scala.tasty.Context) = Term.unapplyTypeApply(impl(arg))(ictx)

  override def unapplySuper(arg: trees.Term)(implicit ctx: scala.tasty.Context) = Term.unapplySuper(impl(arg))(ictx)

  override def unapplyTyped(arg: trees.Term)(implicit ctx: scala.tasty.Context) = Term.unapplyTyped(impl(arg))(ictx)

  override def unapplyAssign(arg: trees.Term)(implicit ctx: scala.tasty.Context) = Term.unapplyAssign(impl(arg))(ictx)

  override def unapplyBlock(arg: trees.Term)(implicit ctx: scala.tasty.Context) = Term.unapplyBlock(impl(arg))(ictx)

  override def unapplyInlined(arg: trees.Term)(implicit ctx: scala.tasty.Context) = Term.unapplyInlined(impl(arg))(ictx)

  override def unapplyLambda(arg: trees.Term)(implicit ctx: scala.tasty.Context) = Term.unapplyLambda(impl(arg))(ictx)

  override def unapplyIf(arg: trees.Term)(implicit ctx: scala.tasty.Context) = Term.unapplyIf(impl(arg))(ictx)

  override def unapplyMatch(arg: trees.Term)(implicit ctx: scala.tasty.Context) = Term.unapplyMatch(impl(arg))(ictx)

  override def unapplyTry(arg: trees.Term)(implicit ctx: scala.tasty.Context) = Term.unapplyTry(impl(arg))(ictx)

  override def unapplyReturn(arg: trees.Term)(implicit ctx: scala.tasty.Context) = Term.unapplyReturn(impl(arg))(ictx)

  override def unapplyRepeated(arg: trees.Term)(implicit ctx: scala.tasty.Context) = Term.unapplyRepeated(impl(arg))(ictx)

  override def unapplySelectOuter(arg: trees.Term)(implicit ctx: scala.tasty.Context) = Term.unapplySelectOuter(impl(arg))(ictx)


  // Pattern

  override def unapplyCaseDef(arg: trees.CaseDef)(implicit ctx: scala.tasty.Context) = CaseDef.unapplyCaseDef(impl(arg))(ictx)

  override def unapplyValue(arg: trees.Pattern)(implicit ctx: scala.tasty.Context) = Pattern.unapplyValue(impl(arg))(ictx)

  override def unapplyBind(arg: trees.Pattern)(implicit ctx: scala.tasty.Context) = Pattern.unapplyBind(impl(arg))(ictx)

  override def unapplyUnapply(arg: trees.Pattern)(implicit ctx: scala.tasty.Context) = Pattern.unapplyUnapply(impl(arg))(ictx)

  override def unapplyAlternative(arg: trees.Pattern)(implicit ctx: scala.tasty.Context) = Pattern.unapplyAlternative(impl(arg))(ictx)

  override def unapplyTypeTest(arg: trees.Pattern)(implicit ctx: scala.tasty.Context) = Pattern.unapplyTypeTest(impl(arg))(ictx)

  // Type trees

  override def unapplySynthetic(arg: trees.TypeTree)(implicit ctx: scala.tasty.Context) = TypeTree.unapplySynthetic(impl(arg))(ictx)

  override def unapplyTypeIdent(arg: trees.TypeTree)(implicit ctx: scala.tasty.Context) = TypeTree.unapplyTypeIdent(impl(arg))(ictx)

  override def unapplyTypeSelect(arg: trees.TypeTree)(implicit ctx: scala.tasty.Context) = TypeTree.unapplyTypeSelect(impl(arg))(ictx)

  override def unapplySingleton(arg: trees.TypeTree)(implicit ctx: scala.tasty.Context) = TypeTree.unapplySingleton(impl(arg))(ictx)

  override def unapplyRefined(arg: trees.TypeTree)(implicit ctx: scala.tasty.Context) = TypeTree.unapplyRefined(impl(arg))(ictx)

  override def unapplyApplied(arg: trees.TypeTree)(implicit ctx: scala.tasty.Context) = TypeTree.unapplyApplied(impl(arg))(ictx)

  override def unapplyAnnotated(arg: trees.TypeTree)(implicit ctx: scala.tasty.Context) = TypeTree.unapplyAnnotated(impl(arg))(ictx)

  override def unapplyAnd(arg: trees.TypeTree)(implicit ctx: scala.tasty.Context) = TypeTree.unapplyAnd(impl(arg))(ictx)

  override def unapplyOr(arg: trees.TypeTree)(implicit ctx: scala.tasty.Context) = TypeTree.unapplyOr(impl(arg))(ictx)

  override def unapplyByName(arg: trees.TypeTree)(implicit ctx: scala.tasty.Context) = TypeTree.unapplyByName(impl(arg))(ictx)

  override def unapplyTypeBoundsTree(arg: trees.TypeBoundsTree)(implicit ctx: scala.tasty.Context) = TypeBoundsTree.unapplyTypeBounds(impl(arg))(ictx)

  // Names

  override def unapplySimple(arg: names.TermName) = TermName.unapplySimple(impl(arg))

  override def unapplyQualified(arg: names.TermName) = TermName.unapplyQualified(impl(arg))

  override def unapplyDefaultGetter(arg: names.TermName) = TermName.unapplyDefaultGetter(impl(arg))

  override def unapplyVariant(arg: names.TermName) = TermName.unapplyVariant(impl(arg))

  override def unapplySuperAccessor(arg: names.TermName) = TermName.unapplySuperAccessor(impl(arg))

  override def unapplyProtectedAccessor(arg: names.TermName) = TermName.unapplyProtectedAccessor(impl(arg))

  override def unapplyProtectedSetter(arg: names.TermName) = TermName.unapplyProtectedSetter(impl(arg))

  override def unapplyObjectClass(arg: names.TermName) = TermName.unapplyObjectClass(impl(arg))

  override def unapplySignedName(arg: names.SignedName) = SignedName.unapplySignedName(impl(arg))

  override def unapplyTypeName(arg: names.TypeName) = TypeName.unapplyTypeName(impl(arg))

  // Constants

  override def unapplyUnit(arg: constants.Constant) = Constant.unapplyUnit(impl(arg))

  override def unapplyNull(arg: constants.Constant) = Constant.unapplyNull(impl(arg))

  override def unapplyBoolean(arg: constants.Constant) = Constant.unapplyBoolean(impl(arg))

  override def unapplyByte(arg: constants.Constant) = Constant.unapplyByte(impl(arg))

  override def unapplyChar(arg: constants.Constant) = Constant.unapplyChar(impl(arg))

  override def unapplyShort(arg: constants.Constant) = Constant.unapplyShort(impl(arg))

  override def unapplyInt(arg: constants.Constant) = Constant.unapplyInt(impl(arg))

  override def unapplyLong(arg: constants.Constant) = Constant.unapplyLong(impl(arg))

  override def unapplyFloat(arg: constants.Constant) = Constant.unapplyFloat(impl(arg))

  override def unapplyDouble(arg: constants.Constant) = Constant.unapplyDouble(impl(arg))

  override def unapplyString(arg: constants.Constant) = Constant.unapplyString(impl(arg))

  // Types

  override def unapplyConstantType(arg: types.Type)(implicit ctx: scala.tasty.Context) = Type.unapplyConstantType(impl(arg))(ictx)

  override def unapplySymRef(arg: types.Type)(implicit ctx: scala.tasty.Context) = Type.unapplySymRef(impl(arg))(ictx)

  override def unapplyNameRef(arg: types.Type)(implicit ctx: scala.tasty.Context) = Type.unapplyNameRef(impl(arg))(ictx)

  override def unapplySuperType(arg: types.Type)(implicit ctx: scala.tasty.Context) = Type.unapplySuperType(impl(arg))(ictx)

  override def unapplyRefinement(arg: types.Type)(implicit ctx: scala.tasty.Context) = Type.unapplyRefinement(impl(arg))(ictx)

  override def unapplyAppliedType(arg: types.Type)(implicit ctx: scala.tasty.Context) = Type.unapplyAppliedType(impl(arg))(ictx)

  override def unapplyAnnotatedType(arg: types.Type)(implicit ctx: scala.tasty.Context) = Type.unapplyAnnotatedType(impl(arg))(ictx)

  override def unapplyAndType(arg: types.Type)(implicit ctx: scala.tasty.Context) = Type.unapplyAndType(impl(arg))(ictx)

  override def unapplyOrType(arg: types.Type)(implicit ctx: scala.tasty.Context) = Type.unapplyOrType(impl(arg))(ictx)

  override def unapplyByNameType(arg: types.Type)(implicit ctx: scala.tasty.Context) = Type.unapplyByNameType(impl(arg))(ictx)

  override def unapplyParamRef(arg: types.Type)(implicit ctx: scala.tasty.Context) = Type.unapplyParamRef(impl(arg))(ictx)

  override def unapplyThisType(arg: types.Type)(implicit ctx: scala.tasty.Context) = Type.unapplyThisType(impl(arg))(ictx)

  override def unapplyRecursiveThis(arg: types.Type)(implicit ctx: scala.tasty.Context) = Type.unapplyRecursiveThis(impl(arg))(ictx)

  override def unapplyRecursiveType(arg: types.RecursiveType)(implicit ctx: scala.tasty.Context) = RecursiveType.unapplyRecursiveType(impl(arg))(ictx)

  override def unapplyMethodType(arg: types.MethodType)(implicit ctx: scala.tasty.Context) = MethodType.unapplyMethodType(impl(arg))(ictx)

  override def unapplyPolyType(arg: types.PolyType)(implicit ctx: scala.tasty.Context) = PolyType.unapplyPolyType(impl(arg))(ictx)

  override def unapplyTypeLambda(arg: types.TypeLambda)(implicit ctx: scala.tasty.Context) = TypeLambda.unapplyTypeLambda(impl(arg))(ictx)

  override def unapplyTypeBounds(arg: types.TypeBounds)(implicit ctx: scala.tasty.Context) = TypeBounds.unapplyTypeBounds(impl(arg))(ictx)

  // Modifiers

  override def unapplyFlags(arg: modifiers.Flags)(implicit ctx: scala.tasty.Context) = FlagsModifier.unapplyFlags(impl(arg))(ictx)

  override def unapplyQualifiedPrivate(arg: modifiers.Qualified)(implicit ctx: scala.tasty.Context) = QualifiedModifier.unapplyQualifiedPrivate(impl(arg))(ictx)

  override def unapplyQualifiedProtected(arg: modifiers.Qualified)(implicit ctx: scala.tasty.Context) = QualifiedModifier.unapplyQualifiedProtected(impl(arg))(ictx)

  override def unapplyAnnotation(arg: modifiers.Annotation)(implicit ctx: scala.tasty.Context) = AnnotationModifier.unapplyAnnotation(impl(arg))(ictx)

  // Import Selectors

  override def unapplySimpleSelector(arg: trees.ImportSelector)(implicit ctx: scala.tasty.Context) = ImportSelector.unapplySimpleSelector(impl(arg))(ictx)

  override def unapplyRenameSelector(arg: trees.ImportSelector)(implicit ctx: scala.tasty.Context) = ImportSelector.unapplyRenameSelector(impl(arg))(ictx)

  override def unapplyOmitSelector(arg: trees.ImportSelector)(implicit ctx: scala.tasty.Context) = ImportSelector.unapplyOmitSelector(impl(arg))(ictx)

  private def ictx(implicit ctx: scala.tasty.Context): Context = {
    val tcxt: TastyContext = impl(ctx)
    tcxt.ctx
  }

  // TODO emit better error message when tasty trait was not implemented by the compiler
  private def impl[T, U <: T](arg: T): U = arg.asInstanceOf[U]
}
