package dotty
package tools.dotc

import scala.language.unsafeNulls

import org.junit.Test
import org.junit.Assert._
import org.junit.experimental.categories.Category
import interfaces._
import scala.collection.mutable.ListBuffer
import java.nio.file._

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
@Category(Array(classOf[BootstrappedOnlyTests]))
class InterfaceEntryPointTest {
  @Test def runCompilerFromInterface = {
    val sources =
      List("tests/pos/HelloWorld.scala").map(p => Paths.get(p).toAbsolutePath().toString)
    val out = Paths.get("out/").toAbsolutePath()
    if (Files.notExists(out))
      Files.createDirectory(out)

    val args = sources ++ List(
      "-d",
      out.toString,
      "-classpath", "", // Avoid the default "."
      "-usejavacp"
    )

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

