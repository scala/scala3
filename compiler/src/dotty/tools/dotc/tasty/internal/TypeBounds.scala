package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types

import scala.tasty.types

object TypeBounds {

  def apply(bounds: Types.TypeBounds)(implicit ctx: Context): types.Type = Impl(bounds, ctx)

  object TypeBounds {
    def unapply(tpe: types.Type): Option[(types.Type, types.Type)] = tpe match {
      case Impl(Types.TypeBounds(lo, hi), ctx) => Some(Type(lo)(ctx), Type(hi)(ctx))
      case _ => None
    }
  }

  private case class Impl(bounds: Types.TypeBounds, ctx: Context) extends types.Type {
    override def toString: String = this match {
      case TypeBounds(lo, hi) => s"TypeBounds($lo, $hi)"
      case _ => s"TypeBounds{## $bounds ##}"
    }
  }
}
