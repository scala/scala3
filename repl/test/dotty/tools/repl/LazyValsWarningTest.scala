package dotty.tools.repl

import java.io.File
import java.nio.charset.StandardCharsets.UTF_8

import scala.io.Source
import scala.util.Using

import org.junit.Assert.{assertFalse, assertTrue}
import org.junit.Assume.assumeTrue
import org.junit.Test

/** Anti-regression test for the `sun.misc.Unsafe`/`LazyVals` warnings
 *
 *  See https://github.com/scala/scala3/issues/25508
 *
 *  The warning is only emitted by the JVM on JDK 24+ (and is to become an error on
 *  a future JDK). When the REPL subprocess runs on the same JVM as the test, the
 *  assertion is therefore skipped on older JDKs. Point the subprocess at a
 *  JDK 24+ to reproduce locally.
 */
class LazyValsWarningTest:

  @Test def noUnsafeLazyValsWarning(): Unit =
    val javaHomeOverride =
      sys.props.get("dotty.tests.replJavaHome").orElse(sys.env.get("DOTTY_REPL_TEST_JAVA_HOME"))
    assumeTrue(
      "warning is only emitted on JDK 24+",
      javaHomeOverride.isDefined || Runtime.version.feature >= 24
    )

    val output = runReplInitScript(javaHomeOverride, "val replCheck = 1 + 1")

    assertTrue(
      s"REPL did not evaluate the init script, output was:\n$output",
      output.contains("replCheck")
    )
    for marker <- List("sun.misc.Unsafe", "objectFieldOffset", "LazyVals") do
      assertFalse(
        s"REPL emitted a legacy lazy vals warning ($marker):\n$output",
        output.contains(marker)
      )

  /** Runs `initScript` in a freshly spawned REPL subprocess and returns its
   *  merged stdout/stderr output.
   */
  private def runReplInitScript(javaHomeOverride: Option[String], initScript: String): String =
    val javaHome = javaHomeOverride.getOrElse(sys.props("java.home"))
    val javaBin = List(javaHome, "bin", "java").mkString(File.separator)
    val classpath = sys.props("java.class.path")
    val command = List(
      javaBin,
      "-Dscala.usejavacp=true",
      "-classpath", classpath,
      "dotty.tools.repl.Main",
      "-usejavacp",
      "-color:never",
      "--repl-quit-after-init",
      "--repl-init-script", initScript,
    )
    val process = ProcessBuilder(command*).redirectErrorStream(true).start()
    val output = Using.resource(Source.fromInputStream(process.getInputStream, UTF_8.name))(_.mkString)
    process.waitFor()
    output
end LazyValsWarningTest
