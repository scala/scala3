package dotty

import scala.language.unsafeNulls

import java.nio.file._

/** Runtime properties from defines or environmnent */
object Properties {

  /** If property is unset or "TRUE" we consider it `true` */
  private def propIsNullOrTrue(name: String): Boolean = {
    val prop = System.getProperty(name)
    prop == null || prop == "TRUE"
  }

  /** If property is unset or FALSE we consider it `false` */
  private def propIsTrue(name: String): Boolean =
    sys.props.getOrElse(name, "FALSE") == "TRUE"

  private def prop(name: String): String =
    sys.props.getOrElse(
      name,
      sys.error(s"Java property $name not set")
    )

  /** Are we running on the CI? */
  val isRunByCI: Boolean = sys.env.isDefinedAt("DOTTY_CI_RUN")

  val testCache: Path =
    sys.env.get("DOTTY_TEST_CACHE").map(Paths.get(_)).getOrElse {
      Paths.get(prop("user.home"), ".cache", "dotty", "test")
    }

  /** Tests should run interactive? */
  val testsInteractive: Boolean = propIsNullOrTrue("dotty.tests.interactive")

  /** Filter out tests not matching the regex supplied by "dotty.tests.filter"
   *  define
   */
  val testsFilter: List[String] = sys.props.get("dotty.tests.filter").fold(Nil)(_.split(',').toList)

  /** Run only failed tests */
  val rerunFailed: Boolean = propIsTrue("dotty.tests.rerunFailed")

  /** Tests should override the checkfiles with the current output */
  val testsUpdateCheckfile: Boolean = propIsTrue("dotty.tests.updateCheckfiles")

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
  def dottyCompilerManagedSources: Path = Paths.get(prop("dotty.tests.dottyCompilerManagedSources"))

  /** dotty-interfaces jar */
  def dottyInterfaces: String = prop("dotty.tests.classes.dottyInterfaces")

  /** dotty-compiler jar */
  def dottyCompiler: String = prop("dotty.tests.classes.dottyCompiler")

  /** dotty-repl jar */
  def dottyRepl: String = prop("dotty.tests.classes.dottyRepl")

  /** dotty-staging jar */
  def dottyStaging: String = prop("dotty.tests.classes.dottyStaging")

  /** dotty-tasty-inspector jar */
  def dottyTastyInspector: String = prop("dotty.tests.classes.dottyTastyInspector")

  /** tasty-core jar */
  def tastyCore: String = prop("dotty.tests.classes.tastyCore")

  /** compiler-interface jar */
  def compilerInterface: String = prop("dotty.tests.classes.compilerInterface")

  /** scala-library jar */
  def scalaLibraryClassPath: String = prop("dotty.tests.classes.scalaLibrary")

  // TODO: Remove this once we migrate the test suite
  def usingScalaLibraryCCTasty: Boolean = true

  // TODO: Remove this once we migrate the test suite
  def usingScalaLibraryTasty: Boolean = true

  /** scala-asm jar */
  def scalaAsm: String = prop("dotty.tests.classes.scalaAsm")

  /** jline-terminal jar */
  def jlineTerminal: String = prop("dotty.tests.classes.jlineTerminal")

  /** jline-reader jar */
  def jlineReader: String = prop("dotty.tests.classes.jlineReader")

  /** scalajs-javalib jar */
  def scalaJSJavalib: String = prop("dotty.tests.classes.scalaJSJavalib")

  /** scalajs-scalalib jar */
  def scalaJSScalalib: String = prop("dotty.tests.classes.scalaJSScalalib")

  /** scalajs-library jar */
  def scalaJSLibrary: String = prop("dotty.tests.classes.scalaJSLibrary")
}
