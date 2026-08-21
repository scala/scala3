package dotty.tools
package repl

import scala.sys.process.*
import scala.util.matching.Regex

import org.junit.Assert.assertEquals
import org.junit.Assume.assumeTrue
import org.junit.Test

class ToolkitTests extends ReplTest:

  private val scalaCliVersion = raw"(?m)^(\d+\.\d+\.\S+)\s*$$".r
  private val replToolkitCoordinate = raw"[^:]+::toolkit:([^:]+)".r

  private case class Toolkit(flavor: String, directive: String, scalaCliDefaultVersion: Regex)

  private val toolkits = List(
    Toolkit("Scala", "default", raw"'default' version for Scala toolkit: ([^,\s]+)".r),
    Toolkit("Typelevel", "typelevel:default", raw"'default' version for typelevel toolkit: ([^,\s]+)".r),
  )

  private def extractVersion(pattern: Regex, output: String, source: String): String =
    pattern.findFirstMatchIn(output).map(_.group(1)) match
      case Some(version) => version.nn
      case None => throw new AssertionError(s"Could not find a version in $source output:\n$output")

  private def replToolkitVersion(directive: String): String =
    val coordinates = ReplDirectives.toolkitCoordinates(directive).getOrElse(Nil)
    val version = coordinates.collectFirst:
      case replToolkitCoordinate(version) => version.nn
    version.getOrElse:
      throw new AssertionError(s"Could not find a toolkit version in REPL coordinates: $coordinates")

  @Test def `default toolkit versions match Scala CLI`: Unit =
    val scalaCliExecutable = sys.env.get("DOTTY_REPL_TEST_SCALA_CLI_EXECUTABLE")
    assumeTrue(
      "Scala CLI integration test requires DOTTY_REPL_TEST_SCALA_CLI_EXECUTABLE",
      scalaCliExecutable.exists(_.nonEmpty)
    )
    val scalaCli = scalaCliExecutable.get
    val expectedScalaCliVersion = sys.props.getOrElse(
      "dotty.tests.scalaCliVersion",
      throw new AssertionError("Missing dotty.tests.scalaCliVersion; run this test through sbt")
    )
    assertEquals(
      "Scala CLI version must match project/Dependencies.scala and .github/workflows/compiler-tests.yaml",
      expectedScalaCliVersion,
      extractVersion(
        scalaCliVersion,
        Seq(scalaCli, "version", "--cli-version").!!,
        "Scala CLI version"
      )
    )

    val scalaCliHelp = Seq(scalaCli, "run", "--help-full").!!
    toolkits.foreach: toolkit =>
      val scalaCliVersion = extractVersion(toolkit.scalaCliDefaultVersion, scalaCliHelp, "Scala CLI help")
      val replVersion = replToolkitVersion(toolkit.directive)
      assertEquals(s"Default ${toolkit.flavor} toolkit version", scalaCliVersion, replVersion)

  @Test def `toolkit command puts the toolkit on the classpath`: Unit =
    initially:
      val stateAfterToolkit = run(":toolkit default")
      storedOutput()
      stateAfterToolkit.andThen:
        run("os.exists(os.pwd)")
        assertEquals("val res0: Boolean = true", storedOutput().trim)

  @Test def `toolkit command rejects anything but a single version`: Unit =
    List(":toolkit", ":toolkit default 0.7.0").foreach: input =>
      initially:
        run(input)
        assertEquals(
          input,
          """:toolkit expects a single version or <flavor>:<version>.
            |Example: :toolkit default""".stripMargin,
          storedOutput().trim
        )
