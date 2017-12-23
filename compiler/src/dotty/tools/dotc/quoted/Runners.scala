package dotty.tools.dotc.quoted

import scala.quoted.Expr
import scala.runtime.quoted._

/** Default runners for quoted expressions */
object Runners {
  implicit def runner[T]: Runner[T] = new Runner[T] {

    def run(expr: Expr[T]): T =
      new QuoteDriver().run(expr)

    def show(expr: Expr[T]): String =
      new QuoteDriver().show(expr)
  }
}
