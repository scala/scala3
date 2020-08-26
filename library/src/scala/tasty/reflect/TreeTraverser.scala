package scala.tasty
package reflect

/** TASTy Reflect tree traverser.
 *
 *  Usage:
 *  ```
 *  class MyTraverser[R <: scala.tasty.Reflection & Singleton](val reflect: R)
 *      extends scala.tasty.reflect.TreeTraverser {
 *    import reflect._
 *    override def traverseTree(tree: Tree): Unit = ...
 *  }
 *  ```
 */
trait TreeTraverser extends TreeAccumulator[Unit] {

  import reflect._

  def traverseTree(tree: Tree): Unit = traverseTreeChildren(tree)

  def foldTree(x: Unit, tree: Tree): Unit = traverseTree(tree)

  protected def traverseTreeChildren(tree: Tree): Unit = foldOverTree((), tree)

}
