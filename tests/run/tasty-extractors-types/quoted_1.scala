import scala.quoted._
import dotty.tools.dotc.quoted.Toolbox._

import scala.tasty.Universe
import scala.tasty.util.{TastyPrinter, TreeTraverser}

object Macros {

  implicit inline def printType[T]: Unit =
    ~impl('[T])(Universe.compilationUniverse) // FIXME infer Universe.compilationUniverse within top level ~

  def impl[T](x: Type[T])(implicit u: Universe): Expr[Unit] = {
    import u._
    import u.tasty._
    val tree = x.toTasty
    val printer = new TastyPrinter(tasty)
    '{
      println(~printer.stringOfTypeTree(tree).toExpr)
      println(~printer.stringOfType(tree.tpe).toExpr)
      println()
    }
  }
}
