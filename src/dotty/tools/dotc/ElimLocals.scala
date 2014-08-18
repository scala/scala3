package dotty.tools.dotc
package transform

import core._
import DenotTransformers.SymTransformer
import Phases.Phase
import Contexts.Context
import SymDenotations.SymDenotation
import TreeTransforms.MiniPhaseTransform
import Flags.Local

/** Widens all private[this] and protected[this] qualifiers to just private/protected */
class ElimLocals extends MiniPhaseTransform with SymTransformer { thisTransformer =>
  override def phaseName = "elimLocals"

  def transformSym(ref: SymDenotation)(implicit ctx: Context) =
    dropLocal(ref)

  private def dropLocal(ref: SymDenotation)(implicit ctx: Context) =
    if (ref.flags is Local) ref.copySymDenotation(initFlags = ref.flags &~ Local)
    else ref
}