package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.transform.MegaPhase._
import dotty.tools.dotc.transform.{PickleQuotes, FirstTransform}
import dotty.tools.dotc.plugins._

/** Set the `defTree` property of symbols for compile plugins
 *  that perform "whole-program" analysis.
 *
 *  All plugins that depend on `symbol.defTree` should sit
 *  between the phase `SetDefTree` and `SetDefTreeOff`.
 */
class SetDefTree extends PluginPhase {
  import tpd._

  override val phaseName: String = SetDefTree.name
  override def runsAfter: Set[String] = Set(PickleQuotes.name)
  override def runsBefore: Set[String] = Set(FirstTransform.name)
    // don't allow plugins to change tasty
    // research plugins can still change the phase plan at will

  override def transformValDef(tree: ValDef)(implicit ctx: Context): Tree = tree.setDefTree

  override def transformDefDef(tree: DefDef)(implicit ctx: Context): Tree = tree.setDefTree

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context): Tree = tree.setDefTree
}

object SetDefTree {
  val name: String = "SetDefTree"
}
