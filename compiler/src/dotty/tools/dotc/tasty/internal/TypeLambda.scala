package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types

import scala.tasty.types

object TypeLambda {

  // TODO make sure all extractors are tested

  def apply(tpe: Types.TypeLambda)(implicit ctx: Context): types.TypeLambda = new Impl(tpe)

  def unapplyTypeLambda(arg: Impl): Option[types.TypeLambda.Data] = {
    implicit val ctx: Context = arg.ctx
    val meth = arg.meth
    Some((meth.paramNames.map(TypeName(_)), meth.paramInfos.map(TypeBounds(_)), Type(meth.resType)))
  }

  private[tasty] class Impl(val meth: Types.TypeLambda)(implicit val ctx: Context) extends types.TypeLambda {
    override def toString: String = {
      import Toolbox.extractor
      val types.MethodType(paramNames, paramTypes, resType) = this
      s"TypeLambda($paramNames, $paramTypes, $resType)"
    }
  }
}
