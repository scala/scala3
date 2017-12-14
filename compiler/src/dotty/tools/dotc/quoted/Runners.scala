package dotty.tools.dotc.quoted

import scala.quoted.Expr
import scala.runtime.quoted._

/** Default runners for quoted expressions */
object Runners {
  implicit def runner[T]: Runner[T] = (expr: Expr[T]) => new QuoteDriver().run(expr)
  implicit def show[T]: Show[T] = (expr: Expr[T]) => new QuoteDriver().show(expr)
}
