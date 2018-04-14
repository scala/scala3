package scala.runtime.tasty

import scala.annotation.implicitNotFound
import scala.tasty._
import scala.tasty.constants.Constant
import scala.tasty.modifiers.Modifier
import scala.tasty.types.MaybeType

@implicitNotFound("Could not find implicit tasty.Toolbox. Default toolbox can be imported with `import dotty.tools.dotc.tasty.Toolbox._`")
trait Toolbox {

  // Statements

  def unapplyPackage(arg: Tree): Option[statements.Package.Data]
  def unapplyImport(arg: Tree): Option[statements.Import.Data]

  // Definitions

  def unapplyValDef(arg: Tree): Option[statements.ValDef.Data]
  def unapplyDefDef(arg: Tree): Option[statements.DefDef.Data]
  def unapplyTypeDef(arg: Tree): Option[statements.TypeDef.Data]
  def unapplyClassDef(arg: Tree): Option[statements.ClassDef.Data]

  // Terms

  def unapplyIdent(arg: Tree): Option[terms.Ident.Data]
  def unapplySelect(arg: Tree): Option[terms.Select.Data]
  def unapplyLiteral(arg: Tree): Option[terms.Literal.Data]
  def unapplyThis(arg: Tree): Option[terms.This.Data]
  def unapplyNew(arg: Tree): Option[terms.New.Data]
  def unapplyNamedArg(arg: Tree): Option[terms.NamedArg.Data]
  def unapplyApply(arg: Tree): Option[terms.Apply.Data]
  def unapplyTypeApply(arg: Tree): Option[terms.TypeApply.Data]
  def unapplySuper(arg: Tree): Option[terms.Super.Data]
  def unapplyTyped(arg: Tree): Option[terms.Typed.Data]
  def unapplyAssign(arg: Tree): Option[terms.Assign.Data]
  def unapplyBlock(arg: Tree): Option[terms.Block.Data]
  def unapplyInlined(arg: Tree): Option[terms.Inlined.Data]
  def unapplyLambda(arg: Tree): Option[terms.Lambda.Data]
  def unapplyIf(arg: Tree): Option[terms.If.Data]
  def unapplyMatch(arg: Tree): Option[terms.Match.Data]
  def unapplyTry(arg: Tree): Option[terms.Try.Data]
  def unapplyReturn(arg: Tree): Option[terms.Return.Data]
  def unapplyRepeated(arg: Tree): Option[terms.Repeated.Data]
  def unapplySelectOuter(arg: Tree): Option[terms.SelectOuter.Data]

  // Patterns

  def unapplyCaseDef(arg: Tree): Option[patterns.CaseDef.Data]

  def unapplyValue(arg: Tree): Option[patterns.Value.Data]
  def unapplyBind(arg: Tree): Option[patterns.Bind.Data]
  def unapplyUnapply(arg: Tree): Option[patterns.Unapply.Data]
  def unapplyAlternative(arg: Tree): Option[patterns.Alternative.Data]
  def unapplyTypeTest(arg: Tree): Option[patterns.TypeTest.Data]

  // Type trees

  def unapplySynthetic(arg: Tree): Boolean
  def unapplyTypeIdent(arg: Tree): Option[typetrees.TypeIdent.Data]
  def unapplyTypeSelect(arg: Tree): Option[typetrees.TypeSelect.Data]
  def unapplySingleton(arg: Tree): Option[typetrees.Singleton.Data]
  def unapplyRefined(arg: Tree): Option[typetrees.Refined.Data]
  def unapplyApplied(arg: Tree): Option[typetrees.Applied.Data]
  def unapplyTypeBoundsTree(arg: Tree): Option[typetrees.TypeBoundsTree.Data]
  def unapplyAnnotated(arg: Tree): Option[typetrees.Annotated.Data]
  def unapplyAnd(arg: Tree): Option[typetrees.And.Data]
  def unapplyOr(arg: Tree): Option[typetrees.Or.Data]
  def unapplyByName(arg: Tree): Option[typetrees.ByName.Data]

  // Names

  def unapplySimple(arg: names.Name): Option[names.Simple.Data]
  def unapplyQualified(arg: names.Name): Option[names.Qualified.Data]

  def unapplyDefaultGetter(arg: names.Name): Option[names.DefaultGetter.Data]
  def unapplyVariant(arg: names.Name): Option[names.Variant.Data]
  def unapplySuperAccessor(arg: names.Name): Option[names.SuperAccessor.Data]
  def unapplyProtectedAccessor(arg: names.Name): Option[names.ProtectedAccessor.Data]
  def unapplyProtectedSetter(arg: names.Name): Option[names.ProtectedSetter.Data]
  def unapplyObjectClass(arg: names.Name): Option[names.ObjectClass.Data]

  def unapplySignedName(arg: names.PossiblySignedName): Option[names.SignedName.Data]

  def unapplyTypeName(arg: names.Name): Option[names.TypeName.Data]

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
  def unapplySymRef(arg: MaybeType): Option[types.SymRef.Data]
  def unapplyNameRef(arg: MaybeType): Option[types.NameRef.Data]
  def unapplySuperType(arg: MaybeType): Option[types.SuperType.Data]
  def unapplyRefinement(arg: MaybeType): Option[types.Refinement.Data]
  def unapplyAppliedType(arg: MaybeType): Option[types.AppliedType.Data]
  def unapplyAnnotatedType(arg: MaybeType): Option[types.AnnotatedType.Data]
  def unapplyAndType(arg: MaybeType): Option[types.AndType.Data]
  def unapplyOrType(arg: MaybeType): Option[types.OrType.Data]
  def unapplyByNameType(arg: MaybeType): Option[types.ByNameType.Data]
  def unapplyParamRef(arg: MaybeType): Option[types.ParamRef.Data]
  def unapplyRecursiveThis(arg: MaybeType): Option[types.RecursiveThis.Data]

  def unapplyRecursiveType(arg: MaybeType): Option[types.RecursiveType.Data]

  def unapplyMethodType(arg: MaybeType): Option[types.MethodType.Data]
  def unapplyPolyType(arg: MaybeType): Option[types.PolyType.Data]
  def unapplyTypeLambda(arg: MaybeType): Option[types.TypeLambda.Data]

  def unapplyTypeBounds(arg: MaybeType): Option[types.TypeBounds.Data]

  // Modifiers

  def unapplyQualifiedPrivate(arg: Modifier): Option[modifiers.QualifiedPrivate.Data]
  def unapplyQualifiedProtected(arg: Modifier): Option[modifiers.QualifiedProtected.Data]
  def unapplyAnnotation(arg: Modifier): Option[modifiers.Annotation.Data]

}
