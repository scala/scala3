package dotty.tools.repl

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
    val javaHomeOverride = ReplTestProcess.javaHomeOverride
    assumeTrue(
      "warning is only emitted on JDK 24+",
      javaHomeOverride.isDefined || Runtime.version.feature >= 24
    )

    val output = ReplTestProcess.output("val replCheck = 1 + 1")

    assertTrue(
      s"REPL did not evaluate the init script, output was:\n$output",
      output.contains("replCheck")
    )
    for marker <- List("sun.misc.Unsafe", "objectFieldOffset", "LazyVals") do
      assertFalse(
        s"REPL emitted a legacy lazy vals warning ($marker):\n$output",
        output.contains(marker)
      )

end LazyValsWarningTest
