package scala.quoted
package staging

import scala.annotation.implicitNotFound

import scala.quoted.runtime.impl.ScopeException

@implicitNotFound("Could not find implicit scala.quoted.staging.Compiler.\n\nDefault compiler can be instantiated with:\n  `import scala.quoted.staging.Compiler; given Compiler = Compiler.make(getClass.getClassLoader)`\n\n")
trait Compiler:
  def run[T](expr: Quotes => Expr[T]): T

object Compiler:

  /** Create a new instance of the compiler using the the classloader of the application.
   *
   *  Usage:
   *  ```
   *  import scala.quoted.staging._
   *  given Compiler =
   *    object Dummy
   *    Compiler.make(Dummy.getClass.getClassLoader)
   *  ```
   *
   *  Note that we use an instance of `Dummy` to get the classloader that loaded the application.
   *  Any other instance of a class defined in the application would also work.
   *  Using a class defined in the standard library should be avoided as it might be loaded by a different classloader.
   *
   *  If the given compiler is defined in one of your classes (e.i. not as a top-level definition), then
   *  the compiler can be instantiated with:
   *  ```
   *  given Compiler = Compiler.make(this.getClass.getClassLoader)
   *  ```
   *
   *  @param appClassloader classloader of the application that generated the quotes
   *  @param settings compiler settings
   *  @return A new instance of the compiler
   */
  def make(appClassloader: ClassLoader)(implicit settings: Settings): Compiler =
    new Compiler:

      private val driver: QuoteDriver = new QuoteDriver(appClassloader)

      private var running = false

      def run[T](exprBuilder: Quotes => Expr[T]): T = synchronized {
        try
          if (running) // detected nested run
            throw new ScopeException("Cannot call `scala.quoted.staging.run(...)` within a another `run(...)`")
          running = true
          driver.run(exprBuilder, settings)
        finally
          running = false
        end try
      }

    end new

  /** Setting of the Compiler instance. */
  case class Settings private (outDir: Option[String], compilerArgs: List[String])

  object Settings:

    implicit def default: Settings = make()

    /** Make compiler settings
     *  @param outDir Output directory for the compiled quote. If set to None the output will be in memory
     *  @param compilerArgs Compiler arguments. Use only if you know what you are doing.
     */
    def make( // TODO avoid using default parameters (for binary compat)
      outDir: Option[String] = None,
      compilerArgs: List[String] = Nil
    ): Settings =
      new Settings(outDir, compilerArgs)

  end Settings

end Compiler
