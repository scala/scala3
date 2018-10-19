import scala.quoted._

import scala.tasty.Tasty
import scala.tasty.util.TreeTraverser

object Macros {

  implicit inline def printTypes[T](x: => T): Unit =
    ~impl('(x))

  def impl[T](x: Expr[T])(implicit tasty: Tasty): Expr[Unit] = {
    import tasty._

    val buff = new StringBuilder
    val traverser = new TreeTraverser(tasty) {
      override def traverseTypeTree(tree: TypeOrBoundsTree)(implicit ctx: Context): Unit = {
        buff.append(tree.tpe.show)
        buff.append("\n\n")
        traverseTypeTreeChildren(tree)
      }
    }

    val tree = x.toTasty
    traverser.traverseTree(tree)
    '(print(~buff.result().toExpr))
  }
}
