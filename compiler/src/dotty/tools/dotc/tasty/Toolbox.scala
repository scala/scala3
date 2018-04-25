package dotty.tools.dotc.tasty

import scala.reflect.ClassTag
import scala.tasty.constants.Constant
import scala.tasty.modifiers.Modifier
import scala.tasty.names
import scala.tasty.trees
import scala.tasty.types

object Toolbox {

  implicit def extractor: scala.runtime.tasty.Toolbox = new scala.runtime.tasty.Toolbox {

    // Statements

    override def unapplyPackageClause(arg: trees.PackageClause) = unapplied(arg, internal.PackageClause.unapplyPackageClause)
    override def unapplyImport(arg: trees.Import) = unapplied(arg, internal.Import.unapplyImport)
    override def unapplyValDef(arg: trees.ValDef) = unapplied(arg, internal.ValDef.unapplyValDef)
    override def unapplyDefDef(arg: trees.DefDef) = unapplied(arg, internal.DefDef.unapplyDefDef)
    override def unapplyTypeDef(arg: trees.TypeDef) = unapplied(arg, internal.TypeDef.unapplyTypeDef)
    override def unapplyClassDef(arg: trees.ClassDef) = unapplied(arg, internal.ClassDef.unapplyClassDef)
    override def unapplyPackageDef(arg: trees.PackageDef) = unapplied(arg, internal.PackageDef.unapplyPackageDef)

    // Terms

    override def unapplyIdent(arg: trees.Term) = unapplied(arg, internal.Term.unapplyIdent)
    override def unapplySelect(arg: trees.Term) = unapplied(arg, internal.Term.unapplySelect)
    override def unapplyLiteral(arg: trees.Term) = unapplied(arg, internal.Term.unapplyLiteral)
    override def unapplyThis(arg: trees.Term) = unapplied(arg, internal.Term.unapplyThis)
    override def unapplyNew(arg: trees.Term) = unapplied(arg, internal.Term.unapplyNew)
    override def unapplyNamedArg(arg: trees.Term) = unapplied(arg, internal.Term.unapplyNamedArg)
    override def unapplyApply(arg: trees.Term) = unapplied(arg, internal.Term.unapplyApply)
    override def unapplyTypeApply(arg: trees.Term) = unapplied(arg, internal.Term.unapplyTypeApply)
    override def unapplySuper(arg: trees.Term) = unapplied(arg, internal.Term.unapplySuper)
    override def unapplyTyped(arg: trees.Term) = unapplied(arg, internal.Term.unapplyTyped)
    override def unapplyAssign(arg: trees.Term) = unapplied(arg, internal.Term.unapplyAssign)
    override def unapplyBlock(arg: trees.Term) = unapplied(arg, internal.Term.unapplyBlock)
    override def unapplyInlined(arg: trees.Term) = unapplied(arg, internal.Term.unapplyInlined)
    override def unapplyLambda(arg: trees.Term) = unapplied(arg, internal.Term.unapplyLambda)
    override def unapplyIf(arg: trees.Term) = unapplied(arg, internal.Term.unapplyIf)
    override def unapplyMatch(arg: trees.Term) = unapplied(arg, internal.Term.unapplyMatch)
    override def unapplyTry(arg: trees.Term) = unapplied(arg, internal.Term.unapplyTry)
    override def unapplyReturn(arg: trees.Term) = unapplied(arg, internal.Term.unapplyReturn)
    override def unapplyRepeated(arg: trees.Term) = unapplied(arg, internal.Term.unapplyRepeated)
    override def unapplySelectOuter(arg: trees.Term) = unapplied(arg, internal.Term.unapplySelectOuter)


    // Pattern

    override def unapplyCaseDef(arg: trees.CaseDef) = unapplied(arg, internal.CaseDef.unapplyCaseDef)

    override def unapplyValue(arg: trees.Pattern) = unapplied(arg, internal.Pattern.unapplyValue)
    override def unapplyBind(arg: trees.Pattern) = unapplied(arg, internal.Pattern.unapplyBind)
    override def unapplyUnapply(arg: trees.Pattern) = unapplied(arg, internal.Pattern.unapplyUnapply)
    override def unapplyAlternative(arg: trees.Pattern) = unapplied(arg, internal.Pattern.unapplyAlternative)
    override def unapplyTypeTest(arg: trees.Pattern) = unapplied(arg, internal.Pattern.unapplyTypeTest)

    // Type trees

    override def unapplySynthetic(arg: trees.TypeTree) = unapplied(arg, internal.TypeTree.unapplySynthetic)
    override def unapplyTypeIdent(arg: trees.TypeTree) = unapplied(arg, internal.TypeTree.unapplyTypeIdent)
    override def unapplyTypeSelect(arg: trees.TypeTree) = unapplied(arg, internal.TypeTree.unapplyTypeSelect)
    override def unapplySingleton(arg: trees.TypeTree) = unapplied(arg, internal.TypeTree.unapplySingleton)
    override def unapplyRefined(arg: trees.TypeTree) = unapplied(arg, internal.TypeTree.unapplyRefined)
    override def unapplyApplied(arg: trees.TypeTree) = unapplied(arg, internal.TypeTree.unapplyApplied)
    override def unapplyAnnotated(arg: trees.TypeTree) = unapplied(arg, internal.TypeTree.unapplyAnnotated)
    override def unapplyAnd(arg: trees.TypeTree) = unapplied(arg, internal.TypeTree.unapplyAnd)
    override def unapplyOr(arg: trees.TypeTree) = unapplied(arg, internal.TypeTree.unapplyOr)
    override def unapplyByName(arg: trees.TypeTree) = unapplied(arg, internal.TypeTree.unapplyByName)

    override def unapplyTypeBoundsTree(arg: trees.TypeBoundsTree) = unapplied(arg, internal.TypeBoundsTree.unapplyTypeBounds)

    // Names

    override def unapplySimple(arg: names.TermName) = unapplied(arg, internal.TermName.unapplySimple)
    override def unapplyQualified(arg: names.TermName) = unapplied(arg, internal.TermName.unapplyQualified)

    override def unapplyDefaultGetter(arg: names.TermName) = unapplied(arg, internal.TermName.unapplyDefaultGetter)
    override def unapplyVariant(arg: names.TermName) = unapplied(arg, internal.TermName.unapplyVariant)
    override def unapplySuperAccessor(arg: names.TermName) = unapplied(arg, internal.TermName.unapplySuperAccessor)
    override def unapplyProtectedAccessor(arg: names.TermName) = unapplied(arg, internal.TermName.unapplyProtectedAccessor)
    override def unapplyProtectedSetter(arg: names.TermName) = unapplied(arg, internal.TermName.unapplyProtectedSetter)
    override def unapplyObjectClass(arg: names.TermName) = unapplied(arg, internal.TermName.unapplyObjectClass)

    override def unapplySignedName(arg: names.SignedName) = unapplied(arg, internal.SignedName.unapplySignedName)

    override def unapplyTypeName(arg: names.TypeName) = unapplied(arg, internal.TypeName.unapplyTypeName)

    // Constants

    override def unapplyUnit(arg: Constant) = unapplied(arg, internal.Constant.unapplyUnit)
    override def unapplyNull(arg: Constant) = unapplied(arg, internal.Constant.unapplyNull)
    override def unapplyBoolean(arg: Constant) = unapplied(arg, internal.Constant.unapplyBoolean)
    override def unapplyByte(arg: Constant) = unapplied(arg, internal.Constant.unapplyByte)
    override def unapplyChar(arg: Constant) = unapplied(arg, internal.Constant.unapplyChar)
    override def unapplyShort(arg: Constant) = unapplied(arg, internal.Constant.unapplyShort)
    override def unapplyInt(arg: Constant) = unapplied(arg, internal.Constant.unapplyInt)
    override def unapplyLong(arg: Constant) = unapplied(arg, internal.Constant.unapplyLong)
    override def unapplyFloat(arg: Constant) = unapplied(arg, internal.Constant.unapplyFloat)
    override def unapplyDouble(arg: Constant) = unapplied(arg, internal.Constant.unapplyDouble)
    override def unapplyString(arg: Constant) = unapplied(arg, internal.Constant.unapplyString)

    // Types

    override def unapplyConstantType(arg: types.Type) = unapplied(arg, internal.Type.unapplyConstantType)
    override def unapplySymRef(arg: types.Type) = unapplied(arg, internal.Type.unapplySymRef)
    override def unapplyNameRef(arg: types.Type) = unapplied(arg, internal.Type.unapplyNameRef)
    override def unapplySuperType(arg: types.Type) = unapplied(arg, internal.Type.unapplySuperType)
    override def unapplyRefinement(arg: types.Type) = unapplied(arg, internal.Type.unapplyRefinement)
    override def unapplyAppliedType(arg: types.Type) = unapplied(arg, internal.Type.unapplyAppliedType)
    override def unapplyAnnotatedType(arg: types.Type) = unapplied(arg, internal.Type.unapplyAnnotatedType)
    override def unapplyAndType(arg: types.Type) = unapplied(arg, internal.Type.unapplyAndType)
    override def unapplyOrType(arg: types.Type) = unapplied(arg, internal.Type.unapplyOrType)
    override def unapplyByNameType(arg: types.Type) = unapplied(arg, internal.Type.unapplyByNameType)
    override def unapplyParamRef(arg: types.Type) = unapplied(arg, internal.Type.unapplyParamRef)
    override def unapplyThisType(arg: types.Type) = unapplied(arg, internal.Type.unapplyThisType)
    override def unapplyRecursiveThis(arg: types.Type) = unapplied(arg, internal.Type.unapplyRecursiveThis)

    override def unapplyRecursiveType(arg: types.RecursiveType) = unapplied(arg, internal.RecursiveType.unapplyRecursiveType)

    override def unapplyMethodType(arg: types.MethodType) = unapplied(arg, internal.MethodType.unapplyMethodType)
    override def unapplyPolyType(arg: types.PolyType) = unapplied(arg, internal.PolyType.unapplyPolyType)
    override def unapplyTypeLambda(arg: types.TypeLambda) = unapplied(arg, internal.TypeLambda.unapplyTypeLambda)

    override def unapplyTypeBounds(arg: types.TypeBounds) = unapplied(arg, internal.TypeBounds.unapplyTypeBounds)

    // Modifiers

    override def unapplyFlags(arg: Modifier) = unapplied(arg, internal.FlagsModifier.unapplyFlags)
    override def unapplyQualifiedPrivate(arg: Modifier) = unapplied(arg, internal.QualifiedModifier.unapplyQualifiedPrivate)
    override def unapplyQualifiedProtected(arg: Modifier) = unapplied(arg, internal.QualifiedModifier.unapplyQualifiedProtected)
    override def unapplyAnnotation(arg: Modifier) = unapplied(arg, internal.AnnotationModifier.unapplyAnnotation)

    // Import Selectors

    override def unapplySimpleSelector(arg: trees.ImportSelector) = unapplied(arg, internal.ImportSelector.unapplySimpleSelector)
    override def unapplyRenameSelector(arg: trees.ImportSelector) = unapplied(arg, internal.ImportSelector.unapplyRenameSelector)
    override def unapplyOmitSelector(arg: trees.ImportSelector) = unapplied(arg, internal.ImportSelector.unapplyOmitSelector)

    private def unapplied[T: ClassTag, U](arg: Any, fn: T => Option[U]): Option[U] = arg match {
      case arg: T => fn(arg)
      case _ => None
    }

    private def unapplied[T: ClassTag](arg: Any, fn: T => Boolean): Boolean = arg match {
      case arg: T => fn(arg)
      case _ => false
    }
  }

}
