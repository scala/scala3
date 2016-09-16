package test

import org.junit.Test
import org.junit.Assert._
import dotty.tools.dotc.Main
import dotty.tools.dotc.interfaces.{CompilerCallback, SourceFile}
import dotty.tools.dotc.reporting._
import dotty.tools.dotc.reporting.diagnostic.Message
import dotty.tools.dotc.core.Contexts._
import java.io.File
import scala.collection.mutable.ListBuffer

/** Test the compiler entry points that depend on dotty
 *
 *  This file also serve as an example for using [[dotty.tools.dotc.Driver#process]].
 *
 *  @see [[InterfaceEntryPointTest]]
 */
class OtherEntryPointsTest {
  @Test def runCompiler = {
    val sources = List("./tests/pos/HelloWorld.scala").map(p => new java.io.File(p).getPath())
    val args = sources ++ List("-d", "./out/")

    val reporter = new CustomReporter
    val callback = new CustomCompilerCallback

    Main.process(args.toArray, reporter, callback)

    assertEquals("Number of errors", false, reporter.hasErrors)
    assertEquals("Number of warnings", false, reporter.hasWarnings)
    assertEquals("Compiled sources", sources, callback.paths)
  }

  @Test def runCompilerWithContext = {
    val sources = List("./tests/pos/HelloWorld.scala").map(p => new java.io.File(p).getPath())
    val args = sources ++ List("-d", "./out/")

    val reporter = new CustomReporter
    val callback = new CustomCompilerCallback
    val context = (new ContextBase).initialCtx.fresh
      .setReporter(reporter)
      .setCompilerCallback(callback)

    Main.process(args.toArray, context)

    assertEquals("Number of errors", false, reporter.hasErrors)
    assertEquals("Number of warnings", false, reporter.hasWarnings)
    assertEquals("Compiled sources", sources, callback.paths)
  }

  private class CustomReporter extends Reporter
      with UniqueMessagePositions
      with HideNonSensicalMessages {
    def doReport(m: Message)(implicit ctx: Context): Unit = {
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
