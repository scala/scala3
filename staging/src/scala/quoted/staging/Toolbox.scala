package scala.quoted
package staging

import scala.annotation.implicitNotFound

@implicitNotFound("Could not find implicit scala.quoted.staging.Toolbox.\n\nDefault toolbox can be instantiated with:\n  `delegate for scala.quoted.staging.Toolbox = scala.quoted.staging.Toolbox.make(getClass.getClassLoader)`\n\n")
trait Toolbox {
  def run[T](expr: QuoteContext => Expr[T]): T
}

object Toolbox {

  /** Create a new instance of the toolbox using the the classloader of the application.
   *
   * Usuage:
   * ```
   * import scala.quoted.staging._
   * delegate for Toolbox = Toolbox.make(getClass.getClassLoader)
   * ```
   *
   * @param appClassloader classloader of the application that generated the quotes
   * @param settings toolbox settings
   * @return A new instance of the toolbox
   */
  def make(appClassloader: ClassLoader)(implicit settings: Settings): Toolbox = new Toolbox {

    private[this] val driver: QuoteDriver = new QuoteDriver(appClassloader)

    private[this] var running = false

    def run[T](exprBuilder: QuoteContext => Expr[T]): T = synchronized {
      try {
        if (running) // detected nested run
          throw new ScopeException("Cannot call `scala.quoted.staging.run(...)` within a another `run(...)`")
        running = true
        driver.run(exprBuilder, settings)
      } finally {
        running = false
      }
    }
  }

  /** Setting of the Toolbox instance. */
  case class Settings private (outDir: Option[String], showRawTree: Boolean, compilerArgs: List[String])

  object Settings {

    implicit def default: Settings = make()

    /** Make toolbox settings
     *  @param outDir Output directory for the compiled quote. If set to None the output will be in memory
     *  @param showRawTree Do not remove quote tree artifacts
     *  @param compilerArgs Compiler arguments. Use only if you know what you are doing.
     */
    def make( // TODO avoid using default parameters (for binary compat)
      showRawTree: Boolean = false,
      outDir: Option[String] = None,
      compilerArgs: List[String] = Nil
    ): Settings =
      new Settings(outDir, showRawTree, compilerArgs)
  }

}
