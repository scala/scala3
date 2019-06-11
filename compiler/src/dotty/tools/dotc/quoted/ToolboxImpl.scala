package dotty.tools.dotc.quoted

import dotty.tools.dotc.ast.tpd

import scala.quoted._
import scala.internal.quoted.{LiftedExpr, TastyTreeExpr}

/** Default runners for quoted expressions */
object ToolboxImpl {
  import tpd._

  /** Create a new instance of the toolbox using the the classloader of the application.
    *
    * @param appClassloader classloader of the application that generated the quotes
    * @param settings toolbox settings
    * @return A new instance of the toolbox
    */
  def make(settings: scala.quoted.Toolbox.Settings, appClassloader: ClassLoader): scala.quoted.Toolbox = new scala.quoted.Toolbox {

    private[this] val driver: QuoteDriver = new QuoteDriver(appClassloader)

    def run[T](expr: Expr[T]): T = expr match {
      case expr: LiftedExpr[T] =>
        expr.value
      case expr: TastyTreeExpr[Tree] @unchecked =>
        throw new Exception("Cannot call `Expr.run` on an `Expr` that comes from a macro argument.")
      case _ =>
        synchronized(driver.run(expr, settings))
    }

    def show[T](expr: Expr[T]): String = synchronized(driver.show(expr, settings))

    def show[T](tpe: Type[T]): String = synchronized(driver.show(tpe, settings))
  }

}
