package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types

import scala.tasty.types

object Type {

  def apply(tpe: Types.Type)(implicit ctx: Context): types.Type = Impl(tpe, ctx)

  def unapplyConstantType(tpe: types.MaybeType): Option[types.ConstantType.Data] = tpe match {
    case Impl(Types.ConstantType(value), _) => Some(Constant(value))
    case _ => None
  }

//  case class SymRef(sym: Definition, qualifier: Type | NoPrefix = NoPrefix) extends Type
//  case class NameRef(name: Name, qualifier: Type | NoPrefix = NoPrefix) extends Type // NoPrefix means: select from _root_

  def unapplySuperType(tpe: types.MaybeType): Option[types.SuperType.Data] = tpe match {
    case Impl(Types.SuperType(thistpe, supertpe), ctx) => Some(Type(thistpe)(ctx), Type(supertpe)(ctx))
    case _ => None
  }

  def unapplyRefinement(tpe: types.MaybeType): Option[types.Refinement.Data] = tpe match {
    case Impl(Types.RefinedType(parent, name, info), ctx) =>
      Some((Type(parent)(ctx), if (name.isTermName) TermName(name.asTermName) else TypeName(name.asTypeName), Type(info)(ctx)))
    case _ => None
  }

  def unapplyAppliedType(tpe: types.MaybeType): Option[types.AppliedType.Data] = tpe match {
    case Impl(Types.AppliedType(tycon, args), ctx) =>
      Some((Type(tycon)(ctx), args.map { case arg: Types.TypeBounds => TypeBounds(arg)(ctx); case arg => Type(arg)(ctx) }))
    case _ => None
  }

  def unapplyAnnotatedType(tpe: types.MaybeType): Option[types.AnnotatedType.Data] = tpe match {
    case Impl(Types.AnnotatedType(underlying, annot), ctx) => Some((Type(underlying)(ctx), Term(annot.tree(ctx))(ctx)))
    case _ => None
  }

  def unapplyAndType(tpe: types.MaybeType): Option[types.AndType.Data] = tpe match {
    case Impl(Types.AndType(left, right), ctx) => Some(Type(left)(ctx), Type(right)(ctx))
    case _ => None
  }

  def unapplyOrType(tpe: types.MaybeType): Option[types.OrType.Data] = tpe match {
    case Impl(Types.OrType(left, right), ctx) => Some(Type(left)(ctx), Type(right)(ctx))
    case _ => None
  }

//  case class ByNameType(underlying: Type) extends Type

//  case class ParamRef(binder: LambdaType[_, _, _], idx: Int) extends Type
  /*
  object ParamRef {
    def unapply(tpe: types.Type): Option[(types.Type, types.Type)] = tpe match {
      case Impl(Types.TypeParamRef(binder, idx), ctx) => Some(TypeLambda(binder)(ctx), idx)
      case _ => None
    }
  }
  */

//  case class RecursiveThis(binder: RecursiveType) extends Type
//
//  case class RecursiveType private (private var _underlying: Type) extends Type {
//    def underlying = _underlying
//  }
//  object RecursiveType {
//    def apply(underlyingExp: RecursiveType => Type) = {
//      val rt = new RecursiveType(PlaceHolder) {}
//      rt._underlying = underlyingExp(rt)
//      rt
//    }
//  }
//
//  abstract class LambdaType[ParamName, ParamInfo, This <: LambdaType[ParamName, ParamInfo, This]](
//                                                                                                   val companion: LambdaTypeCompanion[ParamName, ParamInfo, This]
//                                                                                                 ) {
//    private[Type] var _pinfos: List[ParamInfo]
//    private[Type] var _restpe: Type
//
//    def paramNames: List[ParamName]
//    def paramInfos: List[ParamInfo] = _pinfos
//    def resultType: Type = _restpe
//  }
//
//  abstract class LambdaTypeCompanion[ParamName, ParamInfo, This <: LambdaType[ParamName, ParamInfo, This]] {
//    def apply(pnames: List[ParamName], ptypes: List[ParamInfo], restpe: Type): This
//
//    def apply(pnames: List[ParamName], ptypesExp: This => List[ParamInfo], restpeExp: This => Type): This = {
//      val lambda = apply(pnames, Nil, PlaceHolder)
//      lambda._pinfos = ptypesExp(lambda)
//      lambda._restpe = restpeExp(lambda)
//      lambda
//    }
//  }
//
//  case class MethodType(paramNames: List[TermName], private[Type] var _pinfos: List[Type], private[Type] var _restpe: Type)
//    extends LambdaType[TermName, Type, MethodType](MethodType) {
//    def isImplicit = (companion `eq` ImplicitMethodType) || (companion `eq` ErasedImplicitMethodType)
//    def isErased = (companion `eq` ErasedMethodType) || (companion `eq` ErasedImplicitMethodType)
//  }
//
//  case class PolyType(paramNames: List[TypeName], private[Type] var _pinfos: List[TypeBounds], private[Type] var _restpe: Type)
//    extends LambdaType[TypeName, TypeBounds, PolyType](PolyType)
//
//  case class TypeLambda(paramNames: List[TypeName], private[Type] var _pinfos: List[TypeBounds], private[Type] var _restpe: Type)
//    extends LambdaType[TypeName, TypeBounds, TypeLambda](TypeLambda)
//
//  object TypeLambda extends LambdaTypeCompanion[TypeName, TypeBounds, TypeLambda]
//  object PolyType   extends LambdaTypeCompanion[TypeName, TypeBounds, PolyType]
//  object MethodType extends LambdaTypeCompanion[TermName, Type, MethodType]
//
//  class SpecializedMethodTypeCompanion extends LambdaTypeCompanion[TermName, Type, MethodType] { self =>
//    def apply(pnames: List[TermName], ptypes: List[Type], restpe: Type): MethodType =
//      new MethodType(pnames, ptypes, restpe) { override val companion = self }
//  }
//  object ImplicitMethodType       extends SpecializedMethodTypeCompanion
//  object ErasedMethodType         extends SpecializedMethodTypeCompanion
//  object ErasedImplicitMethodType extends SpecializedMethodTypeCompanion
//
//  case class NoPrefix()
//  object NoPrefix extends NoPrefix

  private case class Impl(tpe: Types.Type, ctx: Context) extends types.Type {
    override def toString: String = {
      import Toolbox.extractor
      this match {
        case types.ConstantType(value) => s"ConstantType($value)"
        case types.SuperType(thisp, superp) => s"SuperType($thisp, $superp)"
        case types.Refinement(parent, name, info) => s"Refinement($parent, $name, $info)"
        case types.AppliedType(tycon, args) => s"AppliedType($tycon, $args)"
        case types.AnnotatedType(underlying, annot) => s"AnnotatedType($underlying, $annot)"
        case types.AndType(left, right) => s"AndType($left, $right)"
        case types.OrType(left, right) => s"OrType($left, $right)"
        case _ => s"Type{## $tpe ##}"
      }
    }
  }

}
