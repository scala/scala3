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

    protected def runImpl[T](code: StagingContext => Expr[T]): T = {
      // TODO check for recursion and throw if possible (i.e. run inside a run)
      synchronized(driver.run(code, settings))
    }

    def show[T](tpe: Type[T]): String = synchronized(driver.show(tpe, settings))
  }

}
