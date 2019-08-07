package dotty.tools.dotc.quoted

import dotty.tools.dotc.core.Contexts.Context

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
          throw new scala.quoted.Toolbox.RunScopeException()
        running = true
        driver.run(exprBuilder, settings)
      } finally {
        running = false
      }
    }
  }

  type ScopeId = Int

  private[dotty] def checkScopeId(id: ScopeId) given Context: Unit = {
    if (id != scopeId)
      throw new Toolbox.RunScopeException
  }

  // TODO Explore more fine grained scope ids.
  //      This id can only differentiate scope extrusion from one compiler instance to another.
  private[dotty] def scopeId given Context: ScopeId =
    the[Context].outersIterator.toList.last.hashCode()

}
