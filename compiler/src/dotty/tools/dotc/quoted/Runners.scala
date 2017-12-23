package dotty.tools.dotc.quoted

import scala.quoted.Expr
import scala.quoted.Liftable.ConstantExpr
import scala.runtime.quoted._

/** Default runners for quoted expressions */
object Runners {

  implicit def runner[T]: Runner[T] = new Runner[T] {

    def run(expr: Expr[T]): T = expr match {
      case expr: ConstantExpr[T] => expr.value
      case _ => new QuoteDriver().run(expr)
    }

    def show(expr: Expr[T]): String = expr match {
      case expr: ConstantExpr[T] => expr.value.toString
      case _ => new QuoteDriver().show(expr)
    }
  }
}
