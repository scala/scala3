package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types

import scala.tasty.types

object MaybeType {

  def apply(tpe: Types.Type)(implicit ctx: Context): types.MaybeType = tpe match {
    case tpe: Types.TypeBounds => TypeBounds(tpe)
    case _ => Type(tpe)
  }

}
