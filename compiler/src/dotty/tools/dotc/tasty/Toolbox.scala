package dotty.tools.dotc.tasty

import scala.tasty.patterns.Pattern
import scala.tasty.statements.TopLevelStatement
import scala.tasty.patterns.CaseDef
import scala.tasty.typetrees.TypeTree
import scala.tasty.constants.Constant
import scala.tasty.names.Name
import scala.tasty.types.MaybeType

object Toolbox {

  implicit def extractor: scala.tasty.Extractor = new scala.tasty.Extractor {

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
    override def unapplyLambda(arg: TopLevelStatement) = internal.Term.unapplyLambda(arg)
    override def unapplyIf(arg: TopLevelStatement) = internal.Term.unapplyIf(arg)
    override def unapplyMatch(arg: TopLevelStatement) = internal.Term.unapplyMatch(arg)
    override def unapplyTry(arg: TopLevelStatement) = internal.Term.unapplyTry(arg)
    override def unapplyReturn(arg: TopLevelStatement) = internal.Term.unapplyReturn(arg)
    override def unapplyRepeated(arg: TopLevelStatement) = internal.Term.unapplyRepeated(arg)

    // Pattern

    override def unapplyCaseDef(arg: CaseDef) = internal.CaseDef.unapplyCaseDef(arg)

    override def unapplyValue(arg: Pattern) = internal.Pattern.unapplyValue(arg)
    override def unapplyBind(arg: Pattern) = internal.Pattern.unapplyBind(arg)
    override def unapplyUnapply(arg: Pattern) = internal.Pattern.unapplyUnapply(arg)
    override def unapplyAlternative(arg: Pattern) = internal.Pattern.unapplyAlternative(arg)
    override def unapplyTypeTest(arg: Pattern) = internal.Pattern.unapplyTypeTest(arg)
    override def unapplyWildcard(arg: Pattern)= internal.Pattern.unapplyWildcard(arg)

    // Types

    override def unapplySynthetic(arg: TypeTree) = internal.TypeTree.Synthetic.unapply(arg)
    override def unapplyIdent(arg: TypeTree) = internal.TypeTree.Ident.unapply(arg)
    override def unapplySelect(arg: TypeTree) = internal.TypeTree.Select.unapply(arg)
    override def unapplySingleton(arg: TypeTree) = internal.TypeTree.Singleton.unapply(arg)
    override def unapplyApplied(arg: TypeTree) = internal.TypeTree.Applied.unapply(arg)
    override def unapplyTypeBounds(arg: TypeTree) = internal.TypeTree.TypeBounds.unapply(arg)
    override def unapplyAnnotated(arg: TypeTree) = internal.TypeTree.Annotated.unapply(arg)
    override def unapplyAnd(arg: TypeTree) = internal.TypeTree.And.unapply(arg)
    override def unapplyOr(arg: TypeTree) = internal.TypeTree.Or.unapply(arg)
    override def unapplyByName(arg: TypeTree) = internal.TypeTree.ByName.unapply(arg)

    // Names

    override def unapplySimple(arg: Name) = internal.TermName.unapplySimple(arg)

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

    override def unapplyTypeBounds(arg: MaybeType)= internal.TypeBounds.unapplyTypeBounds(arg)

  }

}
