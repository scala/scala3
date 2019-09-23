import scala.quoted._
import scala.quoted.matching._

object Macros {

  inline def assignmentPatMat(expr: => Unit): Int =  ${ assignmentPatMatImpl('expr) }

  def assignmentPatMatImpl(expr: Expr[Unit])(given qctx: QuoteContext): Expr[Int] = {
    import qctx.tasty._

    expr match {
      case '{ { $x = 3 } } => '{ 43 }
      case _ =>
        summon[QuoteContext].error("Error")
        '{ ??? }
    }
  }
}
