import scala.quoted._

import scala.tasty._
import scala.tasty.util.TreeTraverser

object Macros {

  implicit rewrite def printOwners[T](x: => T): Unit =
    ~impl('(x))

  def impl[T](x: Expr[T])(implicit tasty: Tasty): Expr[Unit] = {
    import tasty._

    val buff = new StringBuilder

    val output = new TreeTraverser(tasty) {
      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = {
        tree match {
          case IsDefinition(tree @ DefDef(name, _, _, _, _)) =>
            buff.append(name)
            buff.append("\n")
            buff.append(tree.symbol.owner.tree.get.show)
            buff.append("\n\n")
          case IsDefinition(tree @ ValDef(name, _, _)) =>
            buff.append(name)
            buff.append("\n")
            buff.append(tree.symbol.owner.tree.get.show)
            buff.append("\n\n")
          case _ =>
        }
        traverseTreeChildren(tree)
      }
    }

    val tree = x.toTasty
    output.traverseTree(tree)
    '(print(~buff.result().toExpr))
  }

}
