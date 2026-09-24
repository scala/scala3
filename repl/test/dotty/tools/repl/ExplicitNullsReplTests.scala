package dotty.tools
package repl

import scala.language.unsafeNulls

import org.junit.Assert._
import org.junit.Test

/** REPL behaviour with `-Yexplicit-nulls` enabled. */
class ExplicitNullsReplTests
extends ReplTest(ReplTest.defaultOptions ++ Array("-Yexplicit-nulls")) {

  // The "exposes a flexible type" warning is about a library leaking a flexible
  // type into its inferred public API. The synthetic objects the REPL wraps each
  // input in are not an API, so the warning is just noise there.
  @Test def noFlexibleTypeWarningForReplWrapper: Unit = initially:
    run("""val version = System.getProperty("java.version")""")
    val output = storedOutput()
    assertFalse(output, output.contains("flexible type"))
}
