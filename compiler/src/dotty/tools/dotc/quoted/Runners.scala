package dotty.tools.dotc.quoted

import dotty.tools.dotc.ast.Trees.Literal
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.printing.RefinedPrinter

import scala.quoted.Expr
import scala.quoted.Liftable.ConstantExpr
import scala.runtime.quoted._

/** Default runners for quoted expressions */
object Runners {

  implicit def runner[T]: Runner[T] = new Runner[T] {

    def run(expr: Expr[T]): T = Runners.run(expr, optimise = false)

    def show(expr: Expr[T]): String = expr match {
      case expr: ConstantExpr[T] =>
        val ctx = new QuoteDriver().initCtx
        ctx.settings.color.update("never")(ctx)
        val printer = new RefinedPrinter(ctx)
        printer.toText(Literal(Constant(expr.value))).mkString(Int.MaxValue, false)
      case _ => new QuoteDriver().show(expr)
    }
  }

  def run[T](expr: Expr[T], optimise: Boolean): T = expr match {
    case expr: ConstantExpr[T] => expr.value
    case _ => new QuoteDriver().run(expr, optimise)
  }
}
