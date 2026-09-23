package xsbt

import dotty.tools.xsbt.CompilerBridge
import sbt.io.IO
import xsbti.*
import xsbti.compile.SingleOutput

import java.io.File
import java.util.zip.ZipFile
import org.junit.Test
import org.junit.Assert.*

/** Regression test for scala/scala3#27139: a run must not end before its early TASTy is
  * written. A run with only Java sources is the easiest to finish first, so compile one
  * repeatedly.
  */
class AsyncTastyJavaOnlySpecification:

  @Test
  def javaOnlyRunWaitsForEarlyTasty(): Unit =
    for i <- 1 to 5 do
      val temp = IO.createTemporaryDirectory
      val classesDir = new File(temp, "classes")
      classesDir.mkdir()
      val earlyOut = new File(temp, "early.jar")
      val srcFile = new File(temp, "J.java")
      IO.write(srcFile, "public class J { public int x() { return 1; } }")

      val callback = new RecordingCallback
      new CompilerBridge().run(
        Array(new TestVirtualFile(srcFile.toPath)),
        new TestDependencyChanges,
        Array(
          "-classpath", classesDir.getAbsolutePath,
          "-usejavacp",
          "-d", classesDir.getAbsolutePath,
          "-Yforce-sbt-phases",
          "-Xjava-tasty",
          "-Xearly-tasty-output", earlyOut.getAbsolutePath,
        ),
        new SingleOutput { def getOutputDirectory(): File = classesDir },
        callback,
        new TestReporter,
        new TestCompileProgress,
        new TestLogger,
      )

      assertTrue(s"apiPhaseCompleted not called in run $i", callback.apiPhaseCompletedCalled)
      assertTrue(s"dependencyPhaseCompleted not called in run $i", callback.dependencyPhaseCompletedCalled)
      val jar = new ZipFile(earlyOut)
      try assertNotNull(s"J.tasty missing in run $i", jar.getEntry("J.tasty"))
      finally jar.close()

  private class RecordingCallback extends TestCallback:
    @volatile var apiPhaseCompletedCalled = false
    @volatile var dependencyPhaseCompletedCalled = false

    override def apiPhaseCompleted(): Unit = apiPhaseCompletedCalled = true
    override def dependencyPhaseCompleted(): Unit = dependencyPhaseCompletedCalled = true
