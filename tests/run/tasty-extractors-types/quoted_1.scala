import scala.quoted._

import scala.tasty._
import scala.tasty.util.TreeTraverser

object Macros {

  implicit inline def printType[T]: Unit = ~impl('[T])

  def impl[T](x: Type[T])(implicit tasty: Tasty): Expr[Unit] = {
    import tasty._

    val tree = x.toTasty
    '{
      println(~tree.show.toExpr)
      println(~tree.tpe.show.toExpr)
      println()
    }
  }
}
