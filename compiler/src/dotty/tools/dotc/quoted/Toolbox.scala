package dotty.tools.dotc.quoted

import dotty.tools.dotc.ast.tpd

import scala.quoted.Expr
import scala.quoted.Exprs.{LiftedExpr, TastyTreeExpr}

/** Default runners for quoted expressions */
object Toolbox {
  import tpd._

  implicit def make(implicit settings: ToolboxSettings): scala.quoted.Toolbox = new scala.quoted.Toolbox {

    private[this] val driver: QuoteDriver = new QuoteDriver()

    def run[T](expr: Expr[T]): T = expr match {
      case expr: LiftedExpr[T] =>
        expr.value
      case expr: TastyTreeExpr[Tree] @unchecked =>
        throw new Exception("Cannot call `Expr.run` on an `Expr` that comes from a transparent macro argument.")
      case _ =>
        synchronized(driver.run(expr, settings))
    }

    def show[T](expr: Expr[T]): String = synchronized(driver.show(expr, settings))

  }
}
