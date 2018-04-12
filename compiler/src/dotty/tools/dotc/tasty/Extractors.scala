package dotty.tools.dotc.tasty

import scala.tasty.patterns.Pattern
import scala.tasty.statements.TopLevelStatement
import scala.tasty.patterns.CaseDef
import scala.tasty.typetrees.TypeTree
import scala.tasty.constants.Constant
import scala.tasty.names.Name

object Extractors {

  implicit def extractor: scala.tasty.Extractor = new scala.tasty.Extractor {

    // Statements

    override def unapplyPackage(arg: TopLevelStatement) = internal.Package.Package.unapply(arg)
    override def unapplyImport(arg: TopLevelStatement) = internal.Import.Import.unapply(arg)
    override def unapplyValDef(arg: TopLevelStatement) = internal.ValDef.ValDef.unapply(arg)
    override def unapplyDefDef(arg: TopLevelStatement) = internal.DefDef.DefDef.unapply(arg)
    override def unapplyTypeDef(arg: TopLevelStatement) = internal.TypeDef.unapply(arg)
    override def unapplyClassDef(arg: TopLevelStatement) = internal.ClassDef.unapply(arg)

    // Terms

    override def unapplyIdent(arg: TopLevelStatement) = internal.Term.Ident.unapply(arg)
    override def unapplySelect(arg: TopLevelStatement) = internal.Term.Select.unapply(arg)
    override def unapplyLiteral(arg: TopLevelStatement) = internal.Term.Literal.unapply(arg)
    override def unapplyThis(arg: TopLevelStatement) = internal.Term.This.unapply(arg)
    override def unapplyNew(arg: TopLevelStatement) = internal.Term.New.unapply(arg)
    override def unapplyNamedArg(arg: TopLevelStatement) = internal.Term.NamedArg.unapply(arg)
    override def unapplyApply(arg: TopLevelStatement) = internal.Term.Apply.unapply(arg)
    override def unapplyTypeApply(arg: TopLevelStatement) = internal.Term.TypeApply.unapply(arg)
    override def unapplySuper(arg: TopLevelStatement) = internal.Term.Super.unapply(arg)
    override def unapplyTyped(arg: TopLevelStatement) = internal.Term.Typed.unapply(arg)
    override def unapplyAssign(arg: TopLevelStatement) = internal.Term.Assign.unapply(arg)
    override def unapplyBlock(arg: TopLevelStatement) = internal.Term.Block.unapply(arg)
    override def unapplyLambda(arg: TopLevelStatement) = internal.Term.Lambda.unapply(arg)
    override def unapplyIf(arg: TopLevelStatement) = internal.Term.If.unapply(arg)
    override def unapplyMatch(arg: TopLevelStatement) = internal.Term.Match.unapply(arg)
    override def unapplyTry(arg: TopLevelStatement) = internal.Term.Try.unapply(arg)
    override def unapplyReturn(arg: TopLevelStatement) = internal.Term.Return.unapply(arg)
    override def unapplyRepeated(arg: TopLevelStatement) = internal.Term.Repeated.unapply(arg)

    // Case

    override def unapplyCaseDef(arg: CaseDef) = internal.CaseDef.CaseDef.unapply(arg)

    // Pattern

    override def unapplyValue(arg: Pattern) = internal.Pattern.Value.unapply(arg)
    override def unapplyBind(arg: Pattern) = internal.Pattern.Bind.unapply(arg)
    override def unapplyUnapply(arg: Pattern) = internal.Pattern.Unapply.unapply(arg)
    override def unapplyAlternative(arg: Pattern) = internal.Pattern.Alternative.unapply(arg)
    override def unapplyTypeTest(arg: Pattern) = internal.Pattern.TypeTest.unapply(arg)
    override def unapplyWildcard(arg: Pattern)= internal.Pattern.Wildcard.unapply(arg)

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

    override def unapplySimple(arg: Name) = internal.TermName.Simple.unapply(arg)

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
  }

}
