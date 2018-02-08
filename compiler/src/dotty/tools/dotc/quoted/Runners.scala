package dotty.tools.dotc.quoted

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants._
import dotty.tools.dotc.printing.RefinedPrinter

import scala.quoted.Expr
import scala.quoted.Liftable.ConstantExpr
import scala.runtime.BoxedUnit
import scala.runtime.quoted._

/** Default runners for quoted expressions */
object Runners {
  import tpd._

  implicit def runner[T]: Runner[T] = new Runner[T] {

    def run(expr: Expr[T]): T = Runners.run(expr, RunSettings())

    def show(expr: Expr[T]): String = expr match {
      case expr: ConstantExpr[T] =>
        implicit val ctx = new QuoteDriver().initCtx
        ctx.settings.color.update("never")
        val printer = new RefinedPrinter(ctx)
        if (expr.value == BoxedUnit.UNIT) "()"
        else printer.toText(Literal(Constant(expr.value))).mkString(Int.MaxValue, false)
      case _ => new QuoteDriver().show(expr)
    }

    def toConstantOpt(expr: Expr[T]): Option[T] = {
      def toConstantOpt(tree: Tree): Option[T] = tree match {
        case Literal(Constant(c)) => Some(c.asInstanceOf[T])
        case Block(Nil, e) => toConstantOpt(e)
        case Inlined(_, Nil, e) => toConstantOpt(e)
        case _ => None
      }
      expr match {
        case expr: ConstantExpr[T] => Some(expr.value)
        case _ => new QuoteDriver().withTree(expr, (tree, _) => toConstantOpt(tree))
      }
    }

  }

  def run[T](expr: Expr[T], settings: RunSettings): T = expr match {
    case expr: ConstantExpr[T] => expr.value
    case _ => new QuoteDriver().run(expr, settings)
  }

  case class RunSettings(
    /** Enable optimisation when compiling the quoted code */
    optimise: Boolean = false,
    /** Output directory for the copiled quote. If set to None the output will be in memory */
    outDir: Option[String] = None
  )
}
