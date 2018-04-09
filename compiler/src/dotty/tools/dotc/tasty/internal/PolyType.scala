package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types

import scala.tasty.types

object PolyType {

  // TODO make sure all extractors are tested

  def apply(tpe: Types.PolyType): types.PolyType = new Impl(tpe)

  def unapplyPolyType(arg: Impl)(implicit ctx: Context): Option[types.PolyType.Data] = {
    val meth = arg.meth
    Some((meth.paramNames.map(TypeName(_)), meth.paramInfos.map(TypeBounds(_)), Type(meth.resType)))
  }

  private[tasty] class Impl(val meth: Types.PolyType) extends types.PolyType {
    override def toString: String = "PolyType"
  }
}
