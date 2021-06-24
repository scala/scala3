package dotty.tools.dotc
package transform


import dotty.tools.dotc._
import ast.tpd
  import tpd._

import dotty.tools.dotc.core._
import Contexts._
import Types._
import Symbols._

import dotty.tools.dotc.transform._
import MegaPhase._


/** Synchronize the tree definition associated with symbols
 *
 *  This phase will be automatically inserted before a phase if
 *  `phase.synchronizeDefTree == true`.
 *
 */
class SyncDefTree extends MiniPhase {

  val phaseName = "syncDefTree"

  override def transformTypeDef(tree: TypeDef)(using Context): Tree = {
    tree.symbol.defTree = tree
    tree
  }

  override def transformValDef(tree: ValDef)(using Context): Tree = {
    tree.symbol.defTree = tree
    tree
  }

  override def transformDefDef(tree: DefDef)(using Context): Tree = {
    tree.symbol.defTree = tree
    tree
  }
}
