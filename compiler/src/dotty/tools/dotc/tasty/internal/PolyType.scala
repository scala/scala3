package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types

import scala.tasty.types

object PolyType {

  // TODO make sure all extractors are tested

  def apply(tpe: Types.PolyType)(implicit ctx: Context): types.PolyType = Impl(tpe, ctx)

  def unapplyPolyType(tpe: types.MaybeType): Option[types.PolyType.Data] = tpe match {
    case Impl(meth: Types.PolyType, ctx) =>
      implicit val ctx_ = ctx
      Some((meth.paramNames.map(TypeName(_)), meth.paramInfos.map(TypeBounds(_)), Type(meth.resType)))
    case _ => None
  }

  private case class Impl(meth: Types.LambdaType, ctx: Context) extends types.PolyType {
    override def toString: String = {
      import Toolbox.extractor
      this match {
        case types.MethodType(paramNames, paramTypes, resType) =>
          s"MethodType($paramNames, $paramTypes, $resType)"
        case types.PolyType(paramNames, paramBounds, resType) =>
          s"PolyType($paramNames, $paramBounds, $resType)"
        case types.TypeLambda(paramNames, paramBounds, resType) =>
          s"TypeLambda($paramNames, $paramBounds, $resType)"
      }
    }
  }
}
