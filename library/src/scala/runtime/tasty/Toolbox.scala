package scala.runtime.tasty

import scala.annotation.implicitNotFound
import scala.tasty._
import scala.tasty.constants.Constant
import scala.tasty.modifiers.Modifier
import scala.tasty.trees
import scala.tasty.trees.{ImportSelector, Tree}
import scala.tasty.types.MaybeType

@implicitNotFound("Could not find implicit tasty.Toolbox. Default toolbox can be imported with `import dotty.tools.dotc.tasty.Toolbox._`")
trait Toolbox {

  // Statements

  def unapplyPackageDef(arg: Tree): Option[trees.PackageDef.Data]
  def unapplyImport(arg: Tree): Option[trees.Import.Data]

  // Definitions

  def unapplyValDef(arg: Tree): Option[trees.ValDef.Data]
  def unapplyDefDef(arg: Tree): Option[trees.DefDef.Data]
  def unapplyTypeDef(arg: Tree): Option[trees.TypeDef.Data]
  def unapplyClassDef(arg: Tree): Option[trees.ClassDef.Data]

  // Terms

  def unapplyIdent(arg: Tree): Option[trees.Ident.Data]
  def unapplySelect(arg: Tree): Option[trees.Select.Data]
  def unapplyLiteral(arg: Tree): Option[trees.Literal.Data]
  def unapplyThis(arg: Tree): Option[trees.This.Data]
  def unapplyNew(arg: Tree): Option[trees.New.Data]
  def unapplyNamedArg(arg: Tree): Option[trees.NamedArg.Data]
  def unapplyApply(arg: Tree): Option[trees.Apply.Data]
  def unapplyTypeApply(arg: Tree): Option[trees.TypeApply.Data]
  def unapplySuper(arg: Tree): Option[trees.Super.Data]
  def unapplyTyped(arg: Tree): Option[trees.Typed.Data]
  def unapplyAssign(arg: Tree): Option[trees.Assign.Data]
  def unapplyBlock(arg: Tree): Option[trees.Block.Data]
  def unapplyInlined(arg: Tree): Option[trees.Inlined.Data]
  def unapplyLambda(arg: Tree): Option[trees.Lambda.Data]
  def unapplyIf(arg: Tree): Option[trees.If.Data]
  def unapplyMatch(arg: Tree): Option[trees.Match.Data]
  def unapplyTry(arg: Tree): Option[trees.Try.Data]
  def unapplyReturn(arg: Tree): Option[trees.Return.Data]
  def unapplyRepeated(arg: Tree): Option[trees.Repeated.Data]
  def unapplySelectOuter(arg: Tree): Option[trees.SelectOuter.Data]

  // Patterns

  def unapplyCaseDef(arg: Tree): Option[trees.CaseDef.Data]

  def unapplyValue(arg: Tree): Option[trees.Value.Data]
  def unapplyBind(arg: Tree): Option[trees.Bind.Data]
  def unapplyUnapply(arg: Tree): Option[trees.Unapply.Data]
  def unapplyAlternative(arg: Tree): Option[trees.Alternative.Data]
  def unapplyTypeTest(arg: Tree): Option[trees.TypeTest.Data]

  // Type trees

  def unapplySynthetic(arg: Tree): Boolean
  def unapplyTypeIdent(arg: Tree): Option[trees.TypeIdent.Data]
  def unapplyTypeSelect(arg: Tree): Option[trees.TypeSelect.Data]
  def unapplySingleton(arg: Tree): Option[trees.Singleton.Data]
  def unapplyRefined(arg: Tree): Option[trees.Refined.Data]
  def unapplyApplied(arg: Tree): Option[trees.Applied.Data]
  def unapplyTypeBoundsTree(arg: Tree): Option[trees.TypeBoundsTree.Data]
  def unapplyAnnotated(arg: Tree): Option[trees.Annotated.Data]
  def unapplyAnd(arg: Tree): Option[trees.And.Data]
  def unapplyOr(arg: Tree): Option[trees.Or.Data]
  def unapplyByName(arg: Tree): Option[trees.ByName.Data]

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
  def unapplyThisType(arg: MaybeType): Option[types.ThisType.Data]
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

  // Import selectors

  def unapplySimpleSelector(arg: ImportSelector): Option[trees.SimpleSelector.Data]
  def unapplyRenameSelector(arg: ImportSelector): Option[trees.RenameSelector.Data]
  def unapplyOmitSelector(arg: ImportSelector): Option[trees.OmitSelector.Data]


}
