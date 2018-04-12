package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types

import scala.tasty.types

object TypeLambda {

  def apply(tpe: Types.TypeLambda)(implicit ctx: Context): types.TypeLambda = Impl(tpe, ctx)

  def unapplyTypeLambda(tpe: types.MaybeType): Option[types.TypeLambda.Data] = tpe match {
    case Impl(meth: Types.TypeLambda, ctx) =>
      implicit val ctx_ = ctx
      Some((meth.paramNames.map(TypeName(_)), meth.paramInfos.map(TypeBounds(_)), Type(meth.resType)))
    case _ => None
  }

  private case class Impl(meth: Types.TypeLambda, ctx: Context) extends types.TypeLambda {
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
