import scala.quoted._

import scala.tasty._

object Macros {

  implicit inline def printTree[T](x: => T): Unit =
    ~impl('(x))

  def impl[T](x: Expr[T])(implicit tasty: Tasty): Expr[Unit] = {
    import tasty._

    val tree = x.toTasty
    val treeStr = tree.show
    val treeTpeStr = tree.tpe.show

    '{
      println(~treeStr.toExpr)
      println(~treeTpeStr.toExpr)
      println()
    }
  }
}
