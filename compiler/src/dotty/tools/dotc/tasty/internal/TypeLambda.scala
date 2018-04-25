package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types

import scala.tasty.types

object TypeLambda {

  // TODO make sure all extractors are tested

  def apply(tpe: Types.TypeLambda): types.TypeLambda = new Impl(tpe)

  def unapplyTypeLambda(arg: Impl)(implicit ctx: Context): Option[types.TypeLambda.Data] = {
    val meth = arg.meth
    Some((meth.paramNames.map(TypeName(_)), meth.paramInfos.map(TypeBounds(_)), Type(meth.resType)))
  }

  private[tasty] class Impl(val meth: Types.TypeLambda) extends types.TypeLambda {
    override def toString: String = "TypeLambda"
  }
}
