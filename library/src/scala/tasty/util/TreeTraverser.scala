package scala.tasty
package util

import scala.runtime.tasty.Toolbox

abstract class TreeTraverser(implicit toolbox: Toolbox) extends TreeAccumulator[Unit] {

  def traverse(tree: Tree): Unit
  def apply(x: Unit, tree: Tree): Unit = traverse(tree)
  protected def traverseChildren(tree: Tree) = foldOver((), tree)

}
