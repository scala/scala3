package dotty

import java.nio.file._

/** Runtime properties from defines or environmnent */
object Properties {

  /** If property is unset or "TRUE" we consider it `true` */
  private[this] def propIsNullOrTrue(prop: String): Boolean = {
    val prop = System.getProperty("dotty.tests.interactive")
    prop == null || prop == "TRUE"
  }

  /** Are we running on the Drone CI? */
  val isRunByDrone: Boolean = sys.env.isDefinedAt("DRONE")

  /** Tests should run interactive? */
  val testsInteractive: Boolean = propIsNullOrTrue("dotty.tests.interactive")

  /** Filter out tests not matching the regex supplied by "dotty.tests.filter"
   *  define
   */
  val testsFilter: Option[String] = sys.props.get("dotty.tests.filter")

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

  /** dotty-compiler jar */
  def dottyCompiler: String = sys.props("dotty.tests.classes.dottyCompiler")

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
}
