package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types

import scala.tasty.types

object TypeBounds {

  def apply(bounds: Types.TypeBounds)(implicit ctx: Context): types.TypeBounds = Impl(bounds, ctx)

  def unapplyTypeBounds(tpe: types.MaybeType): Option[types.TypeBounds.Data] = tpe match {
    case Impl(Types.TypeBounds(lo, hi), ctx) => Some(Type(lo)(ctx), Type(hi)(ctx))
    case _ => None
  }

  private case class Impl(bounds: Types.TypeBounds, ctx: Context) extends types.TypeBounds {
    override def toString: String = {
      import Toolbox.extractor
      val types.TypeBounds(lo, hi) = this
      s"TypeBounds($lo, $hi)"
    }
  }
}
