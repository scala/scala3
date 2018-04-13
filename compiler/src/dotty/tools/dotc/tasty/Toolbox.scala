package dotty.tools.dotc.tasty

import scala.tasty.patterns.Pattern
import scala.tasty.statements.TopLevelStatement
import scala.tasty.patterns.CaseDef
import scala.tasty.typetrees.MaybeTypeTree
import scala.tasty.constants.Constant
import scala.tasty.modifiers.Modifier
import scala.tasty.names.Name
import scala.tasty.names.PossiblySignedName
import scala.tasty.types.MaybeType

object Toolbox {

  implicit def extractor: scala.runtime.tasty.Toolbox = new scala.runtime.tasty.Toolbox {

    // Statements

    override def unapplyPackage(arg: TopLevelStatement) = internal.Package.unapplyPackage(arg)
    override def unapplyImport(arg: TopLevelStatement) = internal.Import.unapplyImport(arg)
    override def unapplyValDef(arg: TopLevelStatement) = internal.ValDef.unapplyValDef(arg)
    override def unapplyDefDef(arg: TopLevelStatement) = internal.DefDef.unapplyDefDef(arg)
    override def unapplyTypeDef(arg: TopLevelStatement) = internal.TypeDef.unapplyTypeDef(arg)
    override def unapplyClassDef(arg: TopLevelStatement) = internal.ClassDef.unapplyClassDef(arg)

    // Terms

    override def unapplyIdent(arg: TopLevelStatement) = internal.Term.unapplyIdent(arg)
    override def unapplySelect(arg: TopLevelStatement) = internal.Term.unapplySelect(arg)
    override def unapplyLiteral(arg: TopLevelStatement) = internal.Term.unapplyLiteral(arg)
    override def unapplyThis(arg: TopLevelStatement) = internal.Term.unapplyThis(arg)
    override def unapplyNew(arg: TopLevelStatement) = internal.Term.unapplyNew(arg)
    override def unapplyNamedArg(arg: TopLevelStatement) = internal.Term.unapplyNamedArg(arg)
    override def unapplyApply(arg: TopLevelStatement) = internal.Term.unapplyApply(arg)
    override def unapplyTypeApply(arg: TopLevelStatement) = internal.Term.unapplyTypeApply(arg)
    override def unapplySuper(arg: TopLevelStatement) = internal.Term.unapplySuper(arg)
    override def unapplyTyped(arg: TopLevelStatement) = internal.Term.unapplyTyped(arg)
    override def unapplyAssign(arg: TopLevelStatement) = internal.Term.unapplyAssign(arg)
    override def unapplyBlock(arg: TopLevelStatement) = internal.Term.unapplyBlock(arg)
    override def unapplyInlined(arg: TopLevelStatement) = internal.Term.unapplyInlined(arg)
    override def unapplyLambda(arg: TopLevelStatement) = internal.Term.unapplyLambda(arg)
    override def unapplyIf(arg: TopLevelStatement) = internal.Term.unapplyIf(arg)
    override def unapplyMatch(arg: TopLevelStatement) = internal.Term.unapplyMatch(arg)
    override def unapplyTry(arg: TopLevelStatement) = internal.Term.unapplyTry(arg)
    override def unapplyReturn(arg: TopLevelStatement) = internal.Term.unapplyReturn(arg)
    override def unapplyRepeated(arg: TopLevelStatement) = internal.Term.unapplyRepeated(arg)
    override def unapplySelectOuter(arg: TopLevelStatement) = internal.Term.unapplySelectOuter(arg)

    // Pattern

    override def unapplyCaseDef(arg: CaseDef) = internal.CaseDef.unapplyCaseDef(arg)

    override def unapplyValue(arg: Pattern) = internal.Pattern.unapplyValue(arg)
    override def unapplyBind(arg: Pattern) = internal.Pattern.unapplyBind(arg)
    override def unapplyUnapply(arg: Pattern) = internal.Pattern.unapplyUnapply(arg)
    override def unapplyAlternative(arg: Pattern) = internal.Pattern.unapplyAlternative(arg)
    override def unapplyTypeTest(arg: Pattern) = internal.Pattern.unapplyTypeTest(arg)

    // Type trees

    override def unapplySynthetic(arg: MaybeTypeTree) = internal.TypeTree.unapplySynthetic(arg)
    override def unapplyIdent(arg: MaybeTypeTree) = internal.TypeTree.unapplyIdent(arg)
    override def unapplySelect(arg: MaybeTypeTree) = internal.TypeTree.unapplySelect(arg)
    override def unapplySingleton(arg: MaybeTypeTree) = internal.TypeTree.unapplySingleton(arg)
    override def unapplyRefined(arg: MaybeTypeTree) = internal.TypeTree.unapplyRefined(arg)
    override def unapplyApplied(arg: MaybeTypeTree) = internal.TypeTree.unapplyApplied(arg)
    override def unapplyAnnotated(arg: MaybeTypeTree) = internal.TypeTree.unapplyAnnotated(arg)
    override def unapplyAnd(arg: MaybeTypeTree) = internal.TypeTree.unapplyAnd(arg)
    override def unapplyOr(arg: MaybeTypeTree) = internal.TypeTree.unapplyOr(arg)
    override def unapplyByName(arg: MaybeTypeTree) = internal.TypeTree.unapplyByName(arg)

    override def unapplyTypeBoundsTree(arg: MaybeTypeTree) = internal.TypeBoundsTree.unapplyTypeBounds(arg)

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


    override def unapplyUnit(arg: Constant) = internal.Constant.Unit.unapply(arg)
    override def unapplyNull(arg: Constant) = internal.Constant.Null.unapply(arg)
    override def unapplyBoolean(arg: Constant) = internal.Constant.Boolean.unapply(arg)
    override def unapplyByte(arg: Constant) = internal.Constant.Byte.unapply(arg)
    override def unapplyChar(arg: Constant) = internal.Constant.Char.unapply(arg)
    override def unapplyShort(arg: Constant) = internal.Constant.Short.unapply(arg)
    override def unapplyInt(arg: Constant) = internal.Constant.Int.unapply(arg)
    override def unapplyLong(arg: Constant) = internal.Constant.Long.unapply(arg)
    override def unapplyFloat(arg: Constant) = internal.Constant.Float.unapply(arg)
    override def unapplyDouble(arg: Constant) = internal.Constant.Double.unapply(arg)
    override def unapplyString(arg: Constant) = internal.Constant.String.unapply(arg)

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
