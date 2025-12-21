package dotty

import java.nio.file.*
import java.util.regex.{Matcher, Pattern}

/** Runtime properties from defines or environment */
object Properties {

  /** If property is unset or FALSE we consider it `false` */
  private def propIsTrue(name: String): Boolean =
    sys.props.getOrElse(name, "FALSE") == "TRUE"

  /** Are we running on the CI? */
  val isRunByCI: Boolean = sys.env.isDefinedAt("CI")

  /** Whether this is a "quick" run, i.e., slow tests are not run */
  val isQuick: Boolean = propIsTrue("dotty.tests.quick")

  /** Filter out tests not matching the regex supplied by "dotty.tests.filter" define */
  val testsFilter: List[String] = sys.props.get("dotty.tests.filter").fold(Nil)(_.split(',').toList)

  /** Run only failed tests */
  val rerunFailed: Boolean = propIsTrue("dotty.tests.rerunFailed")

  /** Tests should override the checkfiles with the current output */
  val testsUpdateCheckfile: Boolean = propIsTrue("dotty.tests.updateCheckfiles")

  /** Enable Scoverage instrumentation for compilation tests */
  val testsInstrumentCoverage: Boolean = propIsTrue("dotty.tests.instrumentCoverage")

  /** dotty-interfaces jar */
  def dottyInterfaces: String = requireNonEmptyProperty("dotty.tests.classes.dottyInterfaces")

  /** dotty-compiler jar */
  def dottyCompiler: String = requireNonEmptyProperty("dotty.tests.classes.dottyCompiler")

  /** dotty-staging jar */
  def dottyStaging: String = requireNonEmptyProperty("dotty.tests.classes.dottyStaging")

  /** dotty-tasty-inspector jar */
  def dottyTastyInspector: String = requireNonEmptyProperty("dotty.tests.classes.dottyTastyInspector")

  /** tasty-core jar */
  def tastyCore: String = requireNonEmptyProperty("dotty.tests.classes.tastyCore")

  /** compiler-interface jar */
  def compilerInterface: String = requireNonEmptyProperty("dotty.tests.classes.compilerInterface")

  /** scala-library jar */
  def scalaLibrary: String = requireNonEmptyProperty("dotty.tests.classes.scalaLibrary")

  // TODO: Remove this once we migrate the test suite
  def usingScalaLibraryCCTasty: Boolean = true

  // TODO: Remove this once we migrate the test suite
  def usingScalaLibraryTasty: Boolean = true

  /** main ASM jar */
  def asm: String = requireNonEmptyProperty("dotty.tests.classes.asm")

  /** all the ASM jars */
  def asmAll: Seq[String] =
    val sep = java.io.File.separator
    Seq("", "-tree", "-util", "-commons", "-analysis")
      .map(suffix =>
        asm.replaceAll("asm", "asm" + suffix)
          .replaceFirst(Pattern.quote(sep + "asm" + suffix + sep), Matcher.quoteReplacement(sep + "asm" + sep)))

  /** scalajs-javalib jar */
  def scalaJSJavalib: String = requireNonEmptyProperty("dotty.tests.classes.scalaJSJavalib")

  /** scalajs-scalalib jar */
  def scalaJSScalalib: String = requireNonEmptyProperty("dotty.tests.classes.scalaJSScalalib")

  /** scalajs-library jar */
  def scalaJSLibrary: String = requireNonEmptyProperty("dotty.tests.classes.scalaJSLibrary")

  private def requireNonEmptyProperty(name: String): String =
    val value = sys.props(name)
    assert(value != null && value.nonEmpty, s"Property $name is not set")
    value
}
