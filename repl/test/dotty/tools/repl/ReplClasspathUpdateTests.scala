package dotty.tools
package repl

import org.junit.Test
import org.junit.Assert.assertEquals

class ReplClasspathUpdateTests extends ReplTest:
  private def resolveOsLib()(using State): Unit =
    run(":dep com.lihaoyi::os-lib:0.11.8")
    assertEquals("Resolved a dependency (5 JARs)", storedOutput().trim)

  @Test def `i26598 dep after type query does not crash`: Unit =
    initially:
      val stateAfterTypeQuery = run(":type 1")
      storedOutput()
      stateAfterTypeQuery.andThen:
        resolveOsLib()

  @Test def `i26598 dep after doc query does not crash`: Unit =
    initially:
      val stateAfterDocQuery = run(":doc 1")
      storedOutput()
      stateAfterDocQuery.andThen:
        resolveOsLib()

  @Test def `i26598 dep after tab completion does not crash`: Unit =
    initially:
      assertEquals(List("toString"), tabComplete("1.toStr"))
      resolveOsLib()
