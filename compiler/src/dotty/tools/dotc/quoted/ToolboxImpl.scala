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

    def run[T](expr: scala.quoted.QuoteContext => Expr[T]): T = synchronized {
      driver.run(expr, settings)
    }

  }

}
