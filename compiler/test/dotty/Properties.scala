package dotty

import java.nio.file._

/** Runtime properties from defines or environmnent */
object Properties {

  /** If property is unset or "TRUE" we consider it `true` */
  private def propIsNullOrTrue(prop: String): Boolean = {
    val prop = System.getProperty("dotty.tests.interactive")
    prop == null || prop == "TRUE"
  }

  /** Are we running on the CI? */
  val isRunByCI: Boolean = sys.env.isDefinedAt("DOTTY_CI_RUN")
  || sys.env.isDefinedAt("DRONE")  // TODO remove this when we drop Drone

  val testCache: Path =
    sys.env.get("DOTTY_TEST_CACHE").map(Paths.get(_)).getOrElse {
      Paths.get(sys.props("user.home"), ".cache", "dotty", "test")
    }

  /** Tests should run interactive? */
  val testsInteractive: Boolean = propIsNullOrTrue("dotty.tests.interactive")

  /** Filter out tests not matching the regex supplied by "dotty.tests.filter"
   *  define
   */
  val testsFilter: List[String] = sys.props.get("dotty.tests.filter").fold(Nil)(_.split(',').toList)

  /** Tests should override the checkfiles with the current output */
  val testsUpdateCheckfile: Boolean =
    sys.props.getOrElse("dotty.tests.updateCheckfiles", "FALSE") == "TRUE"

  /** When set, the run tests are only compiled - not run, a warning will be
   *  issued
   */
  val testsNoRun: Boolean = sys.props.get("dotty.tests.norun").isDefined

  /** Should Unit tests run in safe mode?
   *
   *  For run tests this means that we respawn child JVM processes after each
   *  test, so that they are never reused.
   */
  val testsSafeMode: Boolean = sys.props.isDefinedAt("dotty.tests.safemode")

  /** Extra directory containing sources for the compiler */
  def dottyCompilerManagedSources: Path = Paths.get(sys.props("dotty.tests.dottyCompilerManagedSources"))

  /** dotty-interfaces jar */
  def dottyInterfaces: String = sys.props("dotty.tests.classes.dottyInterfaces")

  /** dotty-library jar */
  def dottyLibrary: String = sys.props("dotty.tests.classes.dottyLibrary")

  /** dotty-library-js jar */
  def dottyLibraryJS: String = sys.props("dotty.tests.classes.dottyLibraryJS")

  /** dotty-compiler jar */
  def dottyCompiler: String = sys.props("dotty.tests.classes.dottyCompiler")

  /** dotty-staging jar */
  def dottyStaging: String = sys.props("dotty.tests.classes.dottyStaging")

  /** dotty-tasty-inspector jar */
  def dottyTastyInspector: String = sys.props("dotty.tests.classes.dottyTastyInspector")

  /** tasty-core jar */
  def tastyCore: String = sys.props("dotty.tests.classes.tastyCore")

  /** compiler-interface jar */
  def compilerInterface: String = sys.props("dotty.tests.classes.compilerInterface")

  /** scala-library jar */
  def scalaLibrary: String = sys.props("dotty.tests.classes.scalaLibrary")

  /** scala-asm jar */
  def scalaAsm: String = sys.props("dotty.tests.classes.scalaAsm")

  /** jline-terminal jar */
  def jlineTerminal: String = sys.props("dotty.tests.classes.jlineTerminal")

  /** jline-reader jar */
  def jlineReader: String = sys.props("dotty.tests.classes.jlineReader")

  /** scalajs-library jar */
  def scalaJSLibrary: String = sys.props("dotty.tests.classes.scalaJSLibrary")
}
