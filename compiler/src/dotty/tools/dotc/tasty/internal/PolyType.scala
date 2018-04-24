package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types

import scala.tasty.types

object PolyType {

  // TODO make sure all extractors are tested

  def apply(tpe: Types.PolyType)(implicit ctx: Context): types.PolyType = new Impl(tpe)

  def unapplyPolyType(arg: Impl): Option[types.PolyType.Data] = {
    implicit val ctx: Context = arg.ctx
    val meth = arg.meth
    Some((meth.paramNames.map(TypeName(_)), meth.paramInfos.map(TypeBounds(_)), Type(meth.resType)))
  }

  private[tasty] class Impl(val meth: Types.PolyType)(implicit val ctx: Context) extends types.PolyType {
    override def toString: String = {
      import Toolbox.extractor
      val types.PolyType(paramNames, paramTypes, resType) = this
      s"PolyType($paramNames, $paramTypes, $resType)"
    }
  }
}
