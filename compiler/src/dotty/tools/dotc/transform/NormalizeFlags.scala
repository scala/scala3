package dotty.tools.dotc
package transform

import core._
import DenotTransformers.SymTransformer
import Phases.Phase
import Contexts.Context
import SymDenotations.SymDenotation
import TreeTransforms.MiniPhaseTransform
import Flags._, Symbols._

/** 1. Widens all private[this] and protected[this] qualifiers to just private/protected
 *  2. Sets PureInterface flag for traits that only have pure interface members and that
 *     do not have initialization code. A pure interface member is either an abstract
 *     or alias type definition or a deferred val or def.
 */
class NormalizeFlags extends MiniPhaseTransform with SymTransformer { thisTransformer =>
  override def phaseName = "normalizeFlags"

  def transformSym(ref: SymDenotation)(implicit ctx: Context) = {
    var newFlags = ref.flags &~ Local
    if (newFlags != ref.flags) ref.copySymDenotation(initFlags = newFlags)
    else ref
  }
}
