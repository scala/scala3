package dotty.tools.dotc.quoted

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Symbols._

object QuoteUtils:
  import tpd._

  /** Get the owner of a tree if it has one */
  def treeOwner(tree: Tree)(using Context): Option[Symbol] = {
    val getCurrentOwner = new TreeAccumulator[Option[Symbol]] {
      def apply(x: Option[Symbol], tree: tpd.Tree)(using Context): Option[Symbol] =
        if (x.isDefined) x
        else tree match {
          case tree: DefTree => Some(tree.symbol.owner)
          case _ => foldOver(x, tree)
        }
    }
    getCurrentOwner(None, tree)
  }

  /** Changes the owner of the tree based on the current owner of the tree */
  def changeOwnerOfTree(tree: Tree, owner: Symbol)(using Context): Tree = {
    treeOwner(tree) match
      case Some(oldOwner) if oldOwner != owner => tree.changeOwner(oldOwner, owner)
      case _ => tree
  }

end QuoteUtils
