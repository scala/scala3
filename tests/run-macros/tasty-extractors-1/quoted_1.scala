import scala.quoted._
import scala.quoted.autolift._

import scala.tasty._

object Macros {

  implicit inline def printTree[T](x: => T): Unit =
    ${ impl('x) }

  def impl[T](x: Expr[T])(implicit reflect: Reflection): Expr[Unit] = {
    import reflect._

    val tree = x.unseal
    val treeStr = tree.showExtractors
    val treeTpeStr = tree.tpe.showExtractors

    '{
      println(${treeStr})
      println(${treeTpeStr})
      println()
    }
  }
}
