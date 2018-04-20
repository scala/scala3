package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types

import scala.tasty.types

object TypeBounds {

  def apply(bounds: Types.TypeBounds)(implicit ctx: Context): types.TypeBounds = new Impl(bounds)

  def unapplyTypeBounds(arg: Impl): Option[types.TypeBounds.Data] =  {
    implicit val ctx = arg.ctx
    Some(Type(arg.bounds.lo), Type(arg.bounds.hi))
  }

  private[tasty] class Impl(val bounds: Types.TypeBounds)(implicit val ctx: Context) extends types.TypeBounds {
    override def toString: String = {
      import Toolbox.extractor
      val types.TypeBounds(lo, hi) = this
      s"TypeBounds($lo, $hi)"
    }
  }
}
