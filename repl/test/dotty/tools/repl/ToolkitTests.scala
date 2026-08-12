package dotty.tools
package repl

import org.junit.Assert.assertEquals
import org.junit.Test

class ToolkitTests extends ReplTest:

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
          ":toolkit expects a single version, e.g. `:toolkit default`",
          storedOutput().trim
        )
