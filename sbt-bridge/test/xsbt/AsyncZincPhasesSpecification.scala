package xsbt

import dotty.tools.xsbt.CompilerBridge
import sbt.io.IO
import xsbti.*
import xsbti.api.DependencyContext
import xsbti.compile.SingleOutput

import java.io.File
import org.junit.Test
import org.junit.Assert.*

/** With pipelining, Zinc uses `dependencyPhaseCompleted` to write the early
  * analysis, so all dependencies must have been sent by then (scala/scala3#27125).
  */
class AsyncZincPhasesSpecification:

  @Test
  def dependenciesSentBeforeDependencyPhaseCompleted(): Unit =
    val temp = IO.createTemporaryDirectory
    val classesDir = new File(temp, "classes")
    classesDir.mkdir()
    val earlyOut = new File(temp, "early.jar")

    val callback = new SnapshottingCallback
    val srcFile = new File(temp, "Test.scala")
    IO.write(srcFile, "class A; class B extends A; class C { def a: A = new A }")

    new CompilerBridge().run(
      Array(new TestVirtualFile(srcFile.toPath)),
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
      new TestReporter,
      new TestCompileProgress,
      new TestLogger,
    )

    assertTrue("dependencyPhaseCompleted should have been called", callback.depsAtCompletion != null)
    assertTrue(callback.classDependencies.nonEmpty)
    assertEquals(callback.classDependencies.toSet, callback.depsAtCompletion)

  private class SnapshottingCallback extends TestCallback:
    @volatile var depsAtCompletion: Set[(String, String, DependencyContext)] | Null = null

    override def classDependency(onClassName: String, sourceClassName: String, context: DependencyContext): Unit =
      // Slow down sending dependencies, so that an early `dependencyPhaseCompleted` would observe missing ones
      Thread.sleep(50)
      synchronized(super.classDependency(onClassName, sourceClassName, context))

    override def dependencyPhaseCompleted(): Unit =
      depsAtCompletion = synchronized(classDependencies.toSet)
