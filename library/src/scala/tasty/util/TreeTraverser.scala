package scala.tasty.util

import scala.runtime.tasty.Toolbox

import scala.tasty.trees.Tree

abstract class TreeTraverser(implicit toolbox: Toolbox) extends TreeAccumulator[Unit] {

  def traverse(tree: Tree): Unit

  def apply(x: Unit, tree: Tree): Unit = traverse(tree)

  protected def traverseChildren(tree: Tree): Unit = foldOver((), tree)

}
