package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types

import scala.tasty.types

object TypeOrNoPrefix {
  def apply(tpe: Types.Type)(implicit ctx: Context): types.MaybeType =
    if (tpe == Types.NoPrefix) types.NoPrefix
    else Type(tpe)
}
