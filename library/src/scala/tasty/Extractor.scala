package scala.tasty

import scala.tasty.constants.Constant
import scala.tasty.patterns.{CaseDef, Pattern}
import scala.tasty.statements.TopLevelStatement
import scala.tasty.typetrees.TypeTree
import scala.tasty.types.MaybeType

trait Extractor {

  // Statements

  def unapplyPackage(arg: TopLevelStatement): Option[statements.Package.Data]
  def unapplyImport(arg: TopLevelStatement): Option[statements.Import.Data]

  // Definitions

  def unapplyValDef(arg: TopLevelStatement): Option[statements.ValDef.Data]
  def unapplyDefDef(arg: TopLevelStatement): Option[statements.DefDef.Data]
  def unapplyTypeDef(arg: TopLevelStatement): Option[statements.TypeDef.Data]
  def unapplyClassDef(arg: TopLevelStatement): Option[statements.ClassDef.Data]

  // Terms

  def unapplyIdent(arg: TopLevelStatement): Option[terms.Ident.Data]
  def unapplySelect(arg: TopLevelStatement): Option[terms.Select.Data]
  def unapplyLiteral(arg: TopLevelStatement): Option[terms.Literal.Data]
  def unapplyThis(arg: TopLevelStatement): Option[terms.This.Data]
  def unapplyNew(arg: TopLevelStatement): Option[terms.New.Data]
  def unapplyNamedArg(arg: TopLevelStatement): Option[terms.NamedArg.Data]
  def unapplyApply(arg: TopLevelStatement): Option[terms.Apply.Data]
  def unapplyTypeApply(arg: TopLevelStatement): Option[terms.TypeApply.Data]
  def unapplySuper(arg: TopLevelStatement): Option[terms.Super.Data]
  def unapplyTyped(arg: TopLevelStatement): Option[terms.Typed.Data]
  def unapplyAssign(arg: TopLevelStatement): Option[terms.Assign.Data]
  def unapplyBlock(arg: TopLevelStatement): Option[terms.Block.Data]
  def unapplyLambda(arg: TopLevelStatement): Option[terms.Lambda.Data]
  def unapplyIf(arg: TopLevelStatement): Option[terms.If.Data]
  def unapplyMatch(arg: TopLevelStatement): Option[terms.Match.Data]
  def unapplyTry(arg: TopLevelStatement): Option[terms.Try.Data]
  def unapplyReturn(arg: TopLevelStatement): Option[terms.Return.Data]
  def unapplyRepeated(arg: TopLevelStatement): Option[terms.Repeated.Data]

  // Patterns

  def unapplyCaseDef(arg: CaseDef): Option[patterns.CaseDef.Data]

  def unapplyValue(arg: Pattern): Option[patterns.Value.Data]
  def unapplyBind(arg: Pattern): Option[patterns.Bind.Data]
  def unapplyUnapply(arg: Pattern): Option[patterns.Unapply.Data]
  def unapplyAlternative(arg: Pattern): Option[patterns.Alternative.Data]
  def unapplyTypeTest(arg: Pattern): Option[patterns.TypeTest.Data]
  def unapplyWildcard(arg: Pattern): Boolean

  // Type trees

  def unapplySynthetic(arg: TypeTree): Boolean
  def unapplyIdent(arg: TypeTree): Option[typetrees.Ident.Data]
  def unapplySelect(arg: TypeTree): Option[typetrees.Select.Data]
  def unapplySingleton(arg: TypeTree): Option[typetrees.Singleton.Data]
//  def unapplyRefined(arg: TypeTree): Option[typetrees.Refined.Data]
  def unapplyApplied(arg: TypeTree): Option[typetrees.Applied.Data]
  def unapplyTypeBounds(arg: TypeTree): Option[typetrees.TypeBounds.Data]
  def unapplyAnnotated(arg: TypeTree): Option[typetrees.Annotated.Data]
  def unapplyAnd(arg: TypeTree): Option[typetrees.And.Data]
  def unapplyOr(arg: TypeTree): Option[typetrees.Or.Data]
  def unapplyByName(arg: TypeTree): Option[typetrees.ByName.Data]

  // Names

  def unapplySimple(arg: names.Name): Option[String]

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

  def unapplyConstantType(arg: MaybeType): Option[types.ConstantType.Data]
  def unapplySuperType(arg: MaybeType): Option[types.SuperType.Data]
  def unapplyRefinement(arg: MaybeType): Option[types.Refinement.Data]
  def unapplyAppliedType(arg: MaybeType): Option[types.AppliedType.Data]
  def unapplyAnnotatedType(arg: MaybeType): Option[types.AnnotatedType.Data]
  def unapplyAndType(arg: MaybeType): Option[types.AndType.Data]
  def unapplyOrType(arg: MaybeType): Option[types.OrType.Data]

  def unapplyTypeBounds(arg: MaybeType): Option[types.TypeBounds.Data]

}
