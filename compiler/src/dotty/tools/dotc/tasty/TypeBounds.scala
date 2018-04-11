package dotty.tools.dotc.tasty

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types

object TypeBounds {

  def apply(bounds: Types.TypeBounds)(implicit ctx: Context): scala.tasty.Type = Impl(bounds, ctx)

  object TypeBounds {
    def unapply(tpe: scala.tasty.Type): Option[(scala.tasty.Type, scala.tasty.Type)] = tpe match {
      case Impl(Types.TypeBounds(lo, hi), ctx) => Some(Type(lo)(ctx), Type(hi)(ctx))
      case _ => None
    }
  }

  private case class Impl(bounds: Types.TypeBounds, ctx: Context) extends scala.tasty.Type {
    override def toString: String = this match {
      case TypeBounds(lo, hi) => s"TypeBounds($lo, $hi)"
      case _ => s"TypeBounds{## $bounds ##}"
    }
  }
}
