package dotty.tools
package dotc
package transform

import dotty.tools.dotc.util.Positions._
import TreeTransforms.{MiniPhaseTransform, TransformerInfo}
import core._
import Contexts.Context, Types._, Constants._, Decorators._, Symbols._
import TypeUtils._, TypeErasure._, Flags._

class DispatchToSpecializedApply extends MiniPhaseTransform {
  val phaseName = "specializeExtendsFunction1"

  import ast.tpd._
}

