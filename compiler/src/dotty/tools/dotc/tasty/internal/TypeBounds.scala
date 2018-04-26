package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types

import scala.tasty.types

object TypeBounds {

  def apply(bounds: Types.TypeBounds): types.TypeBounds = new Impl(bounds)

  def unapplyTypeBounds(arg: Impl)(implicit ctx: Context): Option[types.TypeBounds.Data] =  {
    Some(Type(arg.bounds.lo), Type(arg.bounds.hi))
  }

  private[tasty] class Impl(val bounds: Types.TypeBounds) extends types.TypeBounds {
    override def toString: String = "TypeBounds"
  }
}
