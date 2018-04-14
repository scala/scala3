package scala.tasty
package util

import scala.runtime.tasty.Toolbox

abstract class TreeTraverser(implicit toolbox: Toolbox) extends TreeAccumulator[Unit] {

  def traverse(tree: statements.TopLevelStatement): Unit
  def traverse(tree: typetrees.MaybeTypeTree): Unit
  def traverse(tree: patterns.CaseDef): Unit
  def traverse(tree: patterns.Pattern): Unit

  def apply(x: Unit, tree: statements.TopLevelStatement): Unit = traverse(tree)
  def apply(x: Unit, tree: typetrees.MaybeTypeTree): Unit = traverse(tree)
  def apply(x: Unit, tree: patterns.CaseDef): Unit = traverse(tree)
  def apply(x: Unit, tree: patterns.Pattern): Unit = traverse(tree)

  protected def traverseChildren(tree: statements.TopLevelStatement) = foldOver((), tree)
  protected def traverseChildren(tree: typetrees.MaybeTypeTree) = foldOver((), tree)
  protected def traverseChildren(tree: patterns.CaseDef) = foldOver((), tree)
  protected def traverseChildren(tree: patterns.Pattern) = foldOver((), tree)

}
