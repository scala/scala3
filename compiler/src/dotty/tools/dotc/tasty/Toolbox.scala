package dotty.tools.dotc.tasty

import scala.reflect.ClassTag
import scala.tasty.constants.Constant
import scala.tasty.modifiers.Modifier
import scala.tasty.names.Name
import scala.tasty.names.PossiblySignedName
import scala.tasty.trees.{ImportSelector, Tree}
import scala.tasty.types.MaybeType

object Toolbox {

  implicit def extractor: scala.runtime.tasty.Toolbox = new scala.runtime.tasty.Toolbox {

    // Statements

    override def unapplyPackageDef(arg: Tree) = unapplied(arg, internal.PackageDef.unapplyPackageDef)
    override def unapplyImport(arg: Tree) = unapplied(arg, internal.Import.unapplyImport)
    override def unapplyValDef(arg: Tree) = unapplied(arg, internal.ValDef.unapplyValDef)
    override def unapplyDefDef(arg: Tree) = unapplied(arg, internal.DefDef.unapplyDefDef)
    override def unapplyTypeDef(arg: Tree) = unapplied(arg, internal.TypeDef.unapplyTypeDef)
    override def unapplyClassDef(arg: Tree) = unapplied(arg, internal.ClassDef.unapplyClassDef)

    // Terms

    override def unapplyIdent(arg: Tree) = unapplied(arg, internal.Term.unapplyIdent)
    override def unapplySelect(arg: Tree) = unapplied(arg, internal.Term.unapplySelect)
    override def unapplyLiteral(arg: Tree) = unapplied(arg, internal.Term.unapplyLiteral)
    override def unapplyThis(arg: Tree) = unapplied(arg, internal.Term.unapplyThis)
    override def unapplyNew(arg: Tree) = unapplied(arg, internal.Term.unapplyNew)
    override def unapplyNamedArg(arg: Tree) = unapplied(arg, internal.Term.unapplyNamedArg)
    override def unapplyApply(arg: Tree) = unapplied(arg, internal.Term.unapplyApply)
    override def unapplyTypeApply(arg: Tree) = unapplied(arg, internal.Term.unapplyTypeApply)
    override def unapplySuper(arg: Tree) = unapplied(arg, internal.Term.unapplySuper)
    override def unapplyTyped(arg: Tree) = unapplied(arg, internal.Term.unapplyTyped)
    override def unapplyAssign(arg: Tree) = unapplied(arg, internal.Term.unapplyAssign)
    override def unapplyBlock(arg: Tree) = unapplied(arg, internal.Term.unapplyBlock)
    override def unapplyInlined(arg: Tree) = unapplied(arg, internal.Term.unapplyInlined)
    override def unapplyLambda(arg: Tree) = unapplied(arg, internal.Term.unapplyLambda)
    override def unapplyIf(arg: Tree) = unapplied(arg, internal.Term.unapplyIf)
    override def unapplyMatch(arg: Tree) = unapplied(arg, internal.Term.unapplyMatch)
    override def unapplyTry(arg: Tree) = unapplied(arg, internal.Term.unapplyTry)
    override def unapplyReturn(arg: Tree) = unapplied(arg, internal.Term.unapplyReturn)
    override def unapplyRepeated(arg: Tree) = unapplied(arg, internal.Term.unapplyRepeated)
    override def unapplySelectOuter(arg: Tree) = unapplied(arg, internal.Term.unapplySelectOuter)


    // Pattern

    override def unapplyCaseDef(arg: Tree) = unapplied(arg, internal.CaseDef.unapplyCaseDef)

    override def unapplyValue(arg: Tree) = unapplied(arg, internal.Pattern.unapplyValue)
    override def unapplyBind(arg: Tree) = unapplied(arg, internal.Pattern.unapplyBind)
    override def unapplyUnapply(arg: Tree) = unapplied(arg, internal.Pattern.unapplyUnapply)
    override def unapplyAlternative(arg: Tree) = unapplied(arg, internal.Pattern.unapplyAlternative)
    override def unapplyTypeTest(arg: Tree) = unapplied(arg, internal.Pattern.unapplyTypeTest)

    // Type trees

    override def unapplySynthetic(arg: Tree) = unapplied(arg, internal.TypeTree.unapplySynthetic)
    override def unapplyTypeIdent(arg: Tree) = unapplied(arg, internal.TypeTree.unapplyTypeIdent)
    override def unapplyTypeSelect(arg: Tree) = unapplied(arg, internal.TypeTree.unapplyTypeSelect)
    override def unapplySingleton(arg: Tree) = unapplied(arg, internal.TypeTree.unapplySingleton)
    override def unapplyRefined(arg: Tree) = unapplied(arg, internal.TypeTree.unapplyRefined)
    override def unapplyApplied(arg: Tree) = unapplied(arg, internal.TypeTree.unapplyApplied)
    override def unapplyAnnotated(arg: Tree) = unapplied(arg, internal.TypeTree.unapplyAnnotated)
    override def unapplyAnd(arg: Tree) = unapplied(arg, internal.TypeTree.unapplyAnd)
    override def unapplyOr(arg: Tree) = unapplied(arg, internal.TypeTree.unapplyOr)
    override def unapplyByName(arg: Tree) = unapplied(arg, internal.TypeTree.unapplyByName)

    override def unapplyTypeBoundsTree(arg: Tree) = unapplied(arg, internal.TypeBoundsTree.unapplyTypeBounds)

    // Names

    override def unapplySimple(arg: Name) = unapplied(arg, internal.TermName.unapplySimple)
    override def unapplyQualified(arg: Name) = unapplied(arg, internal.TermName.unapplyQualified)

    override def unapplyDefaultGetter(arg: Name) = unapplied(arg, internal.TermName.unapplyDefaultGetter)
    override def unapplyVariant(arg: Name) = unapplied(arg, internal.TermName.unapplyVariant)
    override def unapplySuperAccessor(arg: Name) = unapplied(arg, internal.TermName.unapplySuperAccessor)
    override def unapplyProtectedAccessor(arg: Name) = unapplied(arg, internal.TermName.unapplyProtectedAccessor)
    override def unapplyProtectedSetter(arg: Name) = unapplied(arg, internal.TermName.unapplyProtectedSetter)
    override def unapplyObjectClass(arg: Name) = unapplied(arg, internal.TermName.unapplyObjectClass)

    override def unapplySignedName(arg: PossiblySignedName) = unapplied(arg, internal.SignedName.unapplySignedName)

    override def unapplyTypeName(arg: Name) = unapplied(arg, internal.TypeName.unapplyTypeName)

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

    override def unapplyConstantType(arg: MaybeType) = unapplied(arg, internal.Type.unapplyConstantType)
    override def unapplySymRef(arg: MaybeType) = unapplied(arg, internal.Type.unapplySymRef)
    override def unapplyNameRef(arg: MaybeType) = unapplied(arg, internal.Type.unapplyNameRef)
    override def unapplySuperType(arg: MaybeType) = unapplied(arg, internal.Type.unapplySuperType)
    override def unapplyRefinement(arg: MaybeType) = unapplied(arg, internal.Type.unapplyRefinement)
    override def unapplyAppliedType(arg: MaybeType) = unapplied(arg, internal.Type.unapplyAppliedType)
    override def unapplyAnnotatedType(arg: MaybeType) = unapplied(arg, internal.Type.unapplyAnnotatedType)
    override def unapplyAndType(arg: MaybeType) = unapplied(arg, internal.Type.unapplyAndType)
    override def unapplyOrType(arg: MaybeType) = unapplied(arg, internal.Type.unapplyOrType)
    override def unapplyByNameType(arg: MaybeType) = unapplied(arg, internal.Type.unapplyByNameType)
    override def unapplyParamRef(arg: MaybeType) = unapplied(arg, internal.Type.unapplyParamRef)
    override def unapplyThisType(arg: MaybeType) = unapplied(arg, internal.Type.unapplyThisType)
    override def unapplyRecursiveThis(arg: MaybeType) = unapplied(arg, internal.Type.unapplyRecursiveThis)

    override def unapplyRecursiveType(arg: MaybeType) = unapplied(arg, internal.RecursiveType.unapplyRecursiveType)

    override def unapplyMethodType(arg: MaybeType) = unapplied(arg, internal.MethodType.unapplyMethodType)
    override def unapplyPolyType(arg: MaybeType) = unapplied(arg, internal.PolyType.unapplyPolyType)
    override def unapplyTypeLambda(arg: MaybeType) = unapplied(arg, internal.TypeLambda.unapplyTypeLambda)

    override def unapplyTypeBounds(arg: MaybeType) = unapplied(arg, internal.TypeBounds.unapplyTypeBounds)

    // Modifiers

    override def unapplyQualifiedPrivate(arg: Modifier) = unapplied(arg, internal.QualifiedModifier.unapplyQualifiedPrivate)
    override def unapplyQualifiedProtected(arg: Modifier) = unapplied(arg, internal.QualifiedModifier.unapplyQualifiedProtected)
    override def unapplyAnnotation(arg: Modifier) = unapplied(arg, internal.AnnotationModifier.unapplyAnnotation)

    // Import Selectors

    override def unapplySimpleSelector(arg: ImportSelector) = unapplied(arg, internal.ImportSelector.unapplySimpleSelector)
    override def unapplyRenameSelector(arg: ImportSelector) = unapplied(arg, internal.ImportSelector.unapplyRenameSelector)
    override def unapplyOmitSelector(arg: ImportSelector) = unapplied(arg, internal.ImportSelector.unapplyOmitSelector)

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
