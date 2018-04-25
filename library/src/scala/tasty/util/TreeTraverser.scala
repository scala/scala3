package scala.tasty.util

import scala.tasty.Context
import scala.tasty.trees.Tree

abstract class TreeTraverser extends TreeAccumulator[Unit] {

  def traverse(tree: Tree)(implicit ctx: Context): Unit

  def apply(x: Unit, tree: Tree)(implicit ctx: Context): Unit = traverse(tree)

  protected def traverseChildren(tree: Tree)(implicit ctx: Context): Unit = foldOver((), tree)

}
