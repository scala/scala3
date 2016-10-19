package test

import org.junit.Test
import org.junit.Assert._
import dotty.tools.dotc.interfaces._
import scala.collection.mutable.ListBuffer

/** Test that demonstrates how to use dotty-interfaces
 *
 *  This test requires:
 *  - dotty-interfaces to be present at compile-time
 *  - dotty-interfaces and dotty to be present at run-time
 *
 *  Since the ABI of dotty-interfaces is stable, this means that users can write
 *  code that works with multiple versions of dotty without recompilation.
 *
 *  @see [[OtherEntryPointsTest]]
 */
class InterfaceEntryPointTest {
  @Test def runCompilerFromInterface = {
    val sources =
      List("./tests/pos/HelloWorld.scala").map(p => new java.io.File(p).getPath())
    val dottyInterfaces =
      new java.io.File("./interfaces/dotty-interfaces-0.1-SNAPSHOT.jar").getPath
    val dottyLibrary =
      new java.io.File("./library/target/scala-2.11/dotty-library_2.11-0.1-SNAPSHOT.jar").getPath

    val args =
      sources ++
      List("-d", "./out/") ++
      List("-classpath", dottyInterfaces + ":" + dottyLibrary)

    val mainClass = Class.forName("dotty.tools.dotc.Main")
    val process = mainClass.getMethod("process",
      classOf[Array[String]], classOf[SimpleReporter], classOf[CompilerCallback])

    val reporter = new CustomSimpleReporter
    val callback = new CustomCompilerCallback

    // Run the compiler by calling dotty.tools.dotc.Main.process
    process.invoke(null, args.toArray, reporter, callback)

    assertEquals("Number of errors", 0, reporter.errorCount)
    assertEquals("Number of warnings", 0, reporter.warningCount)
    assertEquals("Compiled sources", sources, callback.paths)
  }

  private class CustomSimpleReporter extends SimpleReporter {
    var errorCount = 0
    var warningCount = 0

    def report(diag: Diagnostic): Unit = {
      if (diag.level == Diagnostic.ERROR)
        errorCount += 1
      if (diag.level == Diagnostic.WARNING)
        warningCount += 1

      println(diag.message)
    }
  }

  private class CustomCompilerCallback extends CompilerCallback {
    private val pathsBuffer = new ListBuffer[String]
    def paths = pathsBuffer.toList

    override def onSourceCompiled(source: SourceFile): Unit = {
      if (source.jfile.isPresent)
        pathsBuffer += source.jfile.get.getPath
    }
  }
}

