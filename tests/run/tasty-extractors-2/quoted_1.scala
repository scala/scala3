import scala.quoted._
import dotty.tools.dotc.quoted.Toolbox._

import scala.tasty.Universe
import scala.tasty.util.TastyPrinter

object Macros {

  implicit inline def printTree[T](x: => T): Unit =
    ~impl('(x))(Universe.compilationUniverse) // FIXME infer Universe.compilationUniverse within top level ~

  def impl[T](x: Expr[T])(implicit u: Universe): Expr[Unit] = {
    import u._
    import tasty._
    val tree = x.toTasty
    val treeStr = TastyPrinter.stringOfTree(tasty)(tree)
    val treeTpeStr = TastyPrinter.stringOfType(tasty)(tree.tpe)

    '{
      println(~treeStr.toExpr)
      println(~treeTpeStr.toExpr)
      println()
    }
  }
}
