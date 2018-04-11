package dotty.tools.dotc.tasty

import scala.tasty.statement.TopLevelStatement
import scala.tasty.pattern.CaseDef
import scala.tasty.typetree.TypeTree

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

    // Patterns

    override def unapplyCaseDef(arg: CaseDef) = internal.CaseDef.CaseDef.unapply(arg)

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


  }

}
