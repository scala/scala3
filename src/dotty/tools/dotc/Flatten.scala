package dotty.tools.dotc
package transform

import core._
import DenotTransformers.SymTransformer
import Phases.Phase
import Contexts.Context
import SymDenotations.SymDenotation
import TreeTransforms.MiniPhaseTransform

class Flatten extends MiniPhaseTransform with SymTransformer { thisTransformer =>
  override def phaseName = "flatten"

  def transformSym(ref: SymDenotation)(implicit ctx: Context) = ???
}