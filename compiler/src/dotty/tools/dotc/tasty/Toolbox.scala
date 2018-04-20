package dotty.tools.dotc.tasty

import scala.tasty.constants.Constant
import scala.tasty.modifiers.Modifier
import scala.tasty.names.Name
import scala.tasty.names.PossiblySignedName
import scala.tasty.trees.Tree
import scala.tasty.types.MaybeType

object Toolbox {

  implicit def extractor: scala.runtime.tasty.Toolbox = new scala.runtime.tasty.Toolbox {

    // Statements

    override def unapplyPackageDef(arg: Tree) = internal.PackageDef.unapplyPackageDef(arg)
    override def unapplyImport(arg: Tree) = internal.Import.unapplyImport(arg)
    override def unapplyValDef(arg: Tree) = internal.ValDef.unapplyValDef(arg)
    override def unapplyDefDef(arg: Tree) = internal.DefDef.unapplyDefDef(arg)
    override def unapplyTypeDef(arg: Tree) = internal.TypeDef.unapplyTypeDef(arg)
    override def unapplyClassDef(arg: Tree) = internal.ClassDef.unapplyClassDef(arg)

    // Terms

    override def unapplyIdent(arg: Tree) = internal.Term.unapplyIdent(arg)
    override def unapplySelect(arg: Tree) = internal.Term.unapplySelect(arg)
    override def unapplyLiteral(arg: Tree) = internal.Term.unapplyLiteral(arg)
    override def unapplyThis(arg: Tree) = internal.Term.unapplyThis(arg)
    override def unapplyNew(arg: Tree) = internal.Term.unapplyNew(arg)
    override def unapplyNamedArg(arg: Tree) = internal.Term.unapplyNamedArg(arg)
    override def unapplyApply(arg: Tree) = internal.Term.unapplyApply(arg)
    override def unapplyTypeApply(arg: Tree) = internal.Term.unapplyTypeApply(arg)
    override def unapplySuper(arg: Tree) = internal.Term.unapplySuper(arg)
    override def unapplyTyped(arg: Tree) = internal.Term.unapplyTyped(arg)
    override def unapplyAssign(arg: Tree) = internal.Term.unapplyAssign(arg)
    override def unapplyBlock(arg: Tree) = internal.Term.unapplyBlock(arg)
    override def unapplyInlined(arg: Tree) = internal.Term.unapplyInlined(arg)
    override def unapplyLambda(arg: Tree) = internal.Term.unapplyLambda(arg)
    override def unapplyIf(arg: Tree) = internal.Term.unapplyIf(arg)
    override def unapplyMatch(arg: Tree) = internal.Term.unapplyMatch(arg)
    override def unapplyTry(arg: Tree) = internal.Term.unapplyTry(arg)
    override def unapplyReturn(arg: Tree) = internal.Term.unapplyReturn(arg)
    override def unapplyRepeated(arg: Tree) = internal.Term.unapplyRepeated(arg)
    override def unapplySelectOuter(arg: Tree) = internal.Term.unapplySelectOuter(arg)

    // Pattern

    override def unapplyCaseDef(arg: Tree) = internal.CaseDef.unapplyCaseDef(arg)

    override def unapplyValue(arg: Tree) = internal.Pattern.unapplyValue(arg)
    override def unapplyBind(arg: Tree) = internal.Pattern.unapplyBind(arg)
    override def unapplyUnapply(arg: Tree) = internal.Pattern.unapplyUnapply(arg)
    override def unapplyAlternative(arg: Tree) = internal.Pattern.unapplyAlternative(arg)
    override def unapplyTypeTest(arg: Tree) = internal.Pattern.unapplyTypeTest(arg)

    // Type trees

    override def unapplySynthetic(arg: Tree) = internal.TypeTree.unapplySynthetic(arg)
    override def unapplyTypeIdent(arg: Tree) = internal.TypeTree.unapplyTypeIdent(arg)
    override def unapplyTypeSelect(arg: Tree) = internal.TypeTree.unapplyTypeSelect(arg)
    override def unapplySingleton(arg: Tree) = internal.TypeTree.unapplySingleton(arg)
    override def unapplyRefined(arg: Tree) = internal.TypeTree.unapplyRefined(arg)
    override def unapplyApplied(arg: Tree) = internal.TypeTree.unapplyApplied(arg)
    override def unapplyAnnotated(arg: Tree) = internal.TypeTree.unapplyAnnotated(arg)
    override def unapplyAnd(arg: Tree) = internal.TypeTree.unapplyAnd(arg)
    override def unapplyOr(arg: Tree) = internal.TypeTree.unapplyOr(arg)
    override def unapplyByName(arg: Tree) = internal.TypeTree.unapplyByName(arg)

    override def unapplyTypeBoundsTree(arg: Tree) = internal.TypeBoundsTree.unapplyTypeBounds(arg)

    // Names

    override def unapplySimple(arg: Name) = internal.TermName.unapplySimple(arg)
    override def unapplyQualified(arg: Name) = internal.TermName.unapplyQualified(arg)

    override def unapplyDefaultGetter(arg: Name) = internal.TermName.unapplyDefaultGetter(arg)
    override def unapplyVariant(arg: Name) = internal.TermName.unapplyVariant(arg)
    override def unapplySuperAccessor(arg: Name) = internal.TermName.unapplySuperAccessor(arg)
    override def unapplyProtectedAccessor(arg: Name) = internal.TermName.unapplyProtectedAccessor(arg)
    override def unapplyProtectedSetter(arg: Name) = internal.TermName.unapplyProtectedSetter(arg)
    override def unapplyObjectClass(arg: Name) = internal.TermName.unapplyObjectClass(arg)

    override def unapplySignedName(arg: PossiblySignedName) = internal.SignedName.unapplySignedName(arg)

    override def unapplyTypeName(arg: Name) = internal.TypeName.unapplyTypeName(arg)

    // Constants

    override def unapplyUnit(arg: Constant) = internal.Constant.unapplyUnit(arg)
    override def unapplyNull(arg: Constant) = internal.Constant.unapplyNull(arg)
    override def unapplyBoolean(arg: Constant) = internal.Constant.unapplyBoolean(arg)
    override def unapplyByte(arg: Constant) = internal.Constant.unapplyByte(arg)
    override def unapplyChar(arg: Constant) = internal.Constant.unapplyChar(arg)
    override def unapplyShort(arg: Constant) = internal.Constant.unapplyShort(arg)
    override def unapplyInt(arg: Constant) = internal.Constant.unapplyInt(arg)
    override def unapplyLong(arg: Constant) = internal.Constant.unapplyLong(arg)
    override def unapplyFloat(arg: Constant) = internal.Constant.unapplyFloat(arg)
    override def unapplyDouble(arg: Constant) = internal.Constant.unapplyDouble(arg)
    override def unapplyString(arg: Constant) = internal.Constant.unapplyString(arg)

    // Types

    override def unapplyConstantType(arg: MaybeType) = internal.Type.unapplyConstantType(arg)
    override def unapplySymRef(arg: MaybeType) = internal.Type.unapplySymRef(arg)
    override def unapplyNameRef(arg: MaybeType) = internal.Type.unapplyNameRef(arg)
    override def unapplySuperType(arg: MaybeType) = internal.Type.unapplySuperType(arg)
    override def unapplyRefinement(arg: MaybeType) = internal.Type.unapplyRefinement(arg)
    override def unapplyAppliedType(arg: MaybeType) = internal.Type.unapplyAppliedType(arg)
    override def unapplyAnnotatedType(arg: MaybeType) = internal.Type.unapplyAnnotatedType(arg)
    override def unapplyAndType(arg: MaybeType) = internal.Type.unapplyAndType(arg)
    override def unapplyOrType(arg: MaybeType) = internal.Type.unapplyOrType(arg)
    override def unapplyByNameType(arg: MaybeType) = internal.Type.unapplyByNameType(arg)
    override def unapplyParamRef(arg: MaybeType) = internal.Type.unapplyParamRef(arg)
    override def unapplyThisType(arg: MaybeType) = internal.Type.unapplyThisType(arg)
    override def unapplyRecursiveThis(arg: MaybeType) = internal.Type.unapplyRecursiveThis(arg)

    override def unapplyRecursiveType(arg: MaybeType) = internal.RecursiveType.unapplyRecursiveType(arg)

    override def unapplyMethodType(arg: MaybeType) = internal.MethodType.unapplyMethodType(arg)
    override def unapplyPolyType(arg: MaybeType) = internal.PolyType.unapplyPolyType(arg)
    override def unapplyTypeLambda(arg: MaybeType) = internal.TypeLambda.unapplyTypeLambda(arg)

    override def unapplyTypeBounds(arg: MaybeType)= internal.TypeBounds.unapplyTypeBounds(arg)

    // Modifiers

    override def unapplyQualifiedPrivate(arg: Modifier) = internal.QualifiedModifier.unapplyQualifiedPrivate(arg)
    override def unapplyQualifiedProtected(arg: Modifier) = internal.QualifiedModifier.unapplyQualifiedProtected(arg)
    override def unapplyAnnotation(arg: Modifier) = internal.AnnotationModifier.unapplyAnnotation(arg)

  }

}
