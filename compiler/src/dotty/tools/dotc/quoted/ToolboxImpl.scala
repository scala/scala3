package dotty.tools.dotc.quoted

import scala.quoted._

/** Default runners for quoted expressions */
object ToolboxImpl {

  /** Create a new instance of the toolbox using the the classloader of the application.
    *
    * @param appClassloader classloader of the application that generated the quotes
    * @param settings toolbox settings
    * @return A new instance of the toolbox
    */
  def make(settings: scala.quoted.Toolbox.Settings, appClassloader: ClassLoader): scala.quoted.Toolbox = new scala.quoted.Toolbox {

    private[this] val driver: QuoteDriver = new QuoteDriver(appClassloader)

    private[this] var running = false

    def run[T](exprBuilder: QuoteContext => Expr[T]): T = synchronized {
      try {
        if (running) // detected nested run
          throw new scala.quoted.Toolbox.ToolboxAlreadyRunning()
        running = true
        driver.run(exprBuilder, settings)
      } finally {
        running = false
      }
    }

  }

}
