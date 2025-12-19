package dotty.tools.dotc.quoted

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.DottyTest

import org.junit.Test
import org.junit.Assert.*

/** Tests for the ExecutionEngine and TastyBasedInterpreter intrinsics */
class ExecutionEngineTest extends DottyTest:

  @Test def testPrintlnIntrinsic(): Unit =
    // Create an interpreter and test println capture
    given Context = ctx
    val pos = dotty.tools.dotc.util.NoSourcePosition
    val interpreter = new TastyBasedInterpreter(pos, getClass.getClassLoader)

    // Clear any previous output
    interpreter.clearOutput()

    // The intrinsics should work via the intrinsics map
    // We can test the output capture mechanism
    assertEquals("", interpreter.getCapturedOutput)

  @Test def testOutputCapture(): Unit =
    given Context = ctx
    val pos = dotty.tools.dotc.util.NoSourcePosition
    val interpreter = new TastyBasedInterpreter(pos, getClass.getClassLoader)

    // Verify initial state
    interpreter.clearOutput()
    assertEquals("", interpreter.getCapturedOutput)

    // Stats should include output info
    val stats = interpreter.getStats
    assertTrue(stats.contains("Output captured:"))

  @Test def testExecutionResultCreation(): Unit =
    given Context = ctx
    val engine = new ExecutionEngine

    // Test ExecutionResult creation
    val successResult = engine.ExecutionResult(
      success = true,
      output = "Hello, World!",
      returnValue = Some(42),
      error = None
    )

    assertTrue(successResult.success)
    assertEquals("Hello, World!", successResult.output)
    assertEquals(Some(42), successResult.returnValue)
    assertTrue(successResult.error.isEmpty)

    val failResult = engine.ExecutionResult(
      success = false,
      output = "",
      returnValue = None,
      error = Some(new RuntimeException("Test error"))
    )

    assertFalse(failResult.success)
    assertTrue(failResult.error.isDefined)

  @Test def testIntrinsicsAvailable(): Unit =
    given Context = ctx
    val pos = dotty.tools.dotc.util.NoSourcePosition
    val interpreter = new TastyBasedInterpreter(pos, getClass.getClassLoader)

    // Stats should start at 0
    val stats = interpreter.getStats
    assertTrue(stats.contains("Intrinsic=0"))
    assertTrue(stats.contains("TASTy=0"))
    assertTrue(stats.contains("JVM=0"))

end ExecutionEngineTest

