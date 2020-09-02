package scala.tasty
package reflect

/** TASTy Reflect tree traverser.
 *
 *  Usage:
 *  ```
 *  class MyTraverser[R <: scala.tasty.Reflection & Singleton](val reflect: R)
 *      extends scala.tasty.reflect.TreeTraverser {
 *    import reflect.{given _, _}
 *    override def traverseTree(tree: Tree)(using Owner): Unit = ...
 *  }
 *  ```
 */
trait TreeTraverser extends TreeAccumulator[Unit] {

  import reflect._

  def traverseTree(tree: Tree)(using Owner): Unit = traverseTreeChildren(tree)

  def foldTree(x: Unit, tree: Tree)(using Owner): Unit = traverseTree(tree)

  protected def traverseTreeChildren(tree: Tree)(using Owner): Unit = foldOverTree((), tree)

}
