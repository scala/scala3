package xsbt

import dotty.tools.xsbt.CompilerBridge
import sbt.io.IO
import xsbti.*
import xsbti.compile.SingleOutput

import java.io.File
import org.junit.Test
import org.junit.Assert.*

/** Test that errors from async TASTy Zinc callbacks (apiPhaseCompleted,
  * dependencyPhaseCompleted) are properly relayed to the compiler reporter.
  *
  * This is a regression test for scala/scala3#25774: previously, the sync()
  * call in GenBCode that relayed these reports was removed in #25618, causing
  * errors from Zinc callbacks to be silently lost.
  */
class AsyncTastyErrorRelaySpecification:

  /** When apiPhaseCompleted throws, the error should be reported. */
  @Test
  def asyncTastyErrorIsRelayed(): Unit =
    val src = """class Foo"""
    val temp = IO.createTemporaryDirectory
    val classesDir = new File(temp, "classes")
    classesDir.mkdir()
    val earlyOut = new File(temp, "early.jar")

    val callback = new ThrowingApiCallback
    val reporter = new TestReporter
    val bridge = new CompilerBridge

    val srcFile = new File(temp, "Test.scala")
    IO.write(srcFile, src)
    val virtualSrcFile = new TestVirtualFile(srcFile.toPath)

    try
      bridge.run(
        Array(virtualSrcFile),
        new TestDependencyChanges,
        Array(
          "-classpath", classesDir.getAbsolutePath,
          "-usejavacp",
          "-d", classesDir.getAbsolutePath,
          "-Yforce-sbt-phases",
          "-Yearly-tasty-output", earlyOut.getAbsolutePath,
        ),
        new SingleOutput { def getOutputDirectory(): File = classesDir },
        callback,
        reporter,
        new TestCompileProgress,
        new TestLogger,
      )
    catch case _: CompileFailed => () // expected, since the relayed error causes compilation "failure"

    assertTrue(
      "apiPhaseCompleted should have been called",
      callback.apiPhaseCompletedCalled
    )
    assertTrue(
      "error from apiPhaseCompleted should be reported",
      reporter.problems().exists(p =>
        p.severity == Severity.Error
          && p.message.contains("signaling API and Dependencies phases completion")
      )
    )

  /** A callback that throws in apiPhaseCompleted to simulate a Zinc error. */
  private class ThrowingApiCallback extends TestCallback:
    @volatile var apiPhaseCompletedCalled = false

    override def apiPhaseCompleted(): Unit =
      apiPhaseCompletedCalled = true
      throw new RuntimeException("simulated Zinc apiPhaseCompleted failure")
