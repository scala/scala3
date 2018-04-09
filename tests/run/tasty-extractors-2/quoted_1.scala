import scala.quoted._
import dotty.tools.dotc.quoted.Toolbox._

import scala.tasty.Context
import scala.tasty.util.TastyPrinter

object Macros {

  implicit inline def printTree[T](x: => T): Unit =
    ~impl('(x))(Context.compilationContext) // FIXME infer Context.compilationContext within top level ~

  def impl[T](x: Expr[T])(implicit ctx: Context): Expr[Unit] = {
    val tree = x.toTasty
    val treeStr = TastyPrinter.stringOf(tree)
    val treeTpeStr = TastyPrinter.stringOf(tree.tpe)

    '{
      println(~treeStr.toExpr)
      println(~treeTpeStr.toExpr)
      println()
    }
  }
}
