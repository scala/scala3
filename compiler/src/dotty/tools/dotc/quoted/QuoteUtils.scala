package dotty.tools.dotc.quoted

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Symbols._

object QuoteUtils:
  import tpd._

  /** Get the list of owners of a tree if it has one */
  def treeOwners(tree: Tree)(using Context): List[Symbol] = {
    val getOwners = new TreeAccumulator[Map[Int,Symbol]] {
      def apply(x: Map[Int,Symbol], tree: tpd.Tree)(using Context): Map[Int,Symbol] =
        tree match {
          case tree: DefTree => val owner = tree.symbol.owner
                                x.updated(owner.id, owner)
          case _ => foldOver(x,tree)
        }
    }
    getOwners(Map.empty,tree).values.toList
  }


  /** Changes the owner of the tree based on the current owner of the tree */
  def changeOwnerOfTree(tree: Tree, owner: Symbol)(using Context): Tree = {
    tree.changeOwners(treeOwners(tree), owner)
  }

end QuoteUtils
