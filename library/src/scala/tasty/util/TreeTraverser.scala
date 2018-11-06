package scala.tasty.util

import scala.tasty.Reflection

abstract class TreeTraverser[R <: Reflection with Singleton](reflect0: R) extends TreeAccumulator[Unit, R](reflect0) {
  import reflect.{rootContext => _, _}

  def traverseTree(tree: Tree)(implicit ctx: Context): Unit = traverseTreeChildren(tree)
  def traverseTypeTree(tree: TypeOrBoundsTree)(implicit ctx: Context): Unit = traverseTypeTreeChildren(tree)
  def traverseCaseDef(tree: CaseDef)(implicit ctx: Context): Unit = traverseCaseDefChildren(tree)
  def traversePattern(tree: Pattern)(implicit ctx: Context): Unit = traversePatternChildren(tree)

  def foldTree(x: Unit, tree: Tree)(implicit ctx: Context): Unit = traverseTree(tree)
  def foldTypeTree(x: Unit, tree: TypeOrBoundsTree)(implicit ctx: Context) = traverseTypeTree(tree)
  def foldCaseDef(x: Unit, tree: CaseDef)(implicit ctx: Context) = traverseCaseDef(tree)
  def foldPattern(x: Unit, tree: Pattern)(implicit ctx: Context) = traversePattern(tree)

  protected def traverseTreeChildren(tree: Tree)(implicit ctx: Context): Unit = foldOverTree((), tree)
  protected def traverseTypeTreeChildren(tree: TypeOrBoundsTree)(implicit ctx: Context): Unit = foldOverTypeTree((), tree)
  protected def traverseCaseDefChildren(tree: CaseDef)(implicit ctx: Context): Unit = foldOverCaseDef((), tree)
  protected def traversePatternChildren(tree: Pattern)(implicit ctx: Context): Unit = foldOverPattern((), tree)

}
