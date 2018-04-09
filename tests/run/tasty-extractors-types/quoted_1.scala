import scala.quoted._
import dotty.tools.dotc.quoted.Toolbox._

import scala.tasty.Context
import scala.tasty.names.Name
import scala.tasty.trees._
import scala.tasty.util.{TastyPrinter, TreeTraverser}

object Macros {

  implicit inline def printType[T]: Unit =
    ~impl('[T])(Context.compilationContext) // FIXME infer Context.compilationContext within top level ~

  def impl[T](x: Type[T])(implicit ctx: Context): Expr[Unit] = {
    val tree = x.toTasty
    '{
      println(~TastyPrinter.stringOf(tree).toExpr)
      println(~TastyPrinter.stringOf(tree.tpe).toExpr)
      println()
    }
  }
}
