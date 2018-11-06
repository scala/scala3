import scala.quoted._

import scala.tasty.Reflection
import scala.tasty.util.TreeTraverser

object Macros {

  implicit inline def printTypes[T](x: => T): Unit =
    ~impl('(x))

  def impl[T](x: Expr[T])(implicit reflect: Reflection): Expr[Unit] = {
    import reflect._

    val buff = new StringBuilder
    val traverser = new TreeTraverser(reflect) {
      override def traverseTypeTree(tree: TypeOrBoundsTree)(implicit ctx: Context): Unit = {
        buff.append(tree.tpe.show)
        buff.append("\n\n")
        traverseTypeTreeChildren(tree)
      }
    }

    val tree = x.reflect
    traverser.traverseTree(tree)
    '(print(~buff.result().toExpr))
  }
}
