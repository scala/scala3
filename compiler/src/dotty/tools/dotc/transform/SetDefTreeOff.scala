package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.transform.MegaPhase._

/** Unset the `defTree` property of symbols. See the doc for `SetDefTree` */
class SetDefTreeOff extends MiniPhase {
  import tpd._

  override val phaseName: String = SetDefTreeOff.name
  override def runsAfter: Set[String] = Set(SetDefTree.name)

  override def transformValDef(tree: ValDef)(implicit ctx: Context): Tree = {
    tree.symbol.defTree = EmptyTree
    tree
  }

  override def transformDefDef(tree: DefDef)(implicit ctx: Context): Tree = {
    tree.symbol.defTree = EmptyTree
    tree
  }

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context): Tree = {
    tree.symbol.defTree = EmptyTree
    tree
  }
}

object SetDefTreeOff {
  val name: String = "SetDefTreeOff"
}
