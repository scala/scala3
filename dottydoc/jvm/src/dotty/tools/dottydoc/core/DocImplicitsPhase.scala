package dotty.tools
package dottydoc
package core

import dotty.tools.dotc.transform.TreeTransforms.{ MiniPhaseTransform, TransformerInfo }
import dotty.tools.dotc.core.Flags
import dotc.core.Contexts.Context

class DocImplicitsPhase extends MiniPhaseTransform { thisTransformer =>
  import dotty.tools.dotc.ast.tpd._

  def phaseName = "addImplicitsPhase"

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (
      tree.symbol.is(Flags.Implicit)  && // has to have an implicit flag
      tree.symbol.owner.isStaticOwner && // owner has to be static (e.g. top-level `object`)
      tree.vparamss.length > 0        &&
      tree.vparamss(0).length == 1       // should only take one arg, since it has to be a transformation
    ) {
      val convertee = tree.vparamss(0)(0).symbol.info.widenDealias.finalResultType.typeSymbol//denot.typeRef // the pimped type (i.e. `class`)
      ctx.base.addDef(convertee, tree.symbol.info.widenDealias.finalResultType.typeSymbol)
    }

    tree
  }
}
