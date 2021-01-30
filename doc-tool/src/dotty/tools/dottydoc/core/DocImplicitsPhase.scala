package dotty.tools
package dottydoc
package core

import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.core.Flags
import dotc.core.Contexts.{Context, ctx}
import util.syntax._

class DocImplicitsPhase extends MiniPhase {
  import dotty.tools.dotc.ast.tpd._

  def phaseName = "addImplicitsPhase"

  override def transformDefDef(tree: DefDef)(using Context): Tree = {
    if tree.symbol.isOneOf(Flags.GivenOrImplicit) // has to have an implicit flag
       && tree.symbol.owner.isStaticOwner         // owner has to be static (e.g. top-level `object`)
       && tree.termParamss.length > 0
       && tree.termParamss(0).length == 1         // should only take one arg, since it has to be a transformation
    then
      val convertee = tree.termParamss(0)(0).symbol.info.widenDealias.finalResultType.typeSymbol // the pimped type (i.e. `class`)
      ctx.docbase.addDef(convertee, tree.symbol.info.widenDealias.finalResultType.typeSymbol)

    tree
  }
}
