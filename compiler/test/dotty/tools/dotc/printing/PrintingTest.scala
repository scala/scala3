package dotty
package tools
package dotc
package printing

import dotty.tools.io.FileExtension
import vulpix.FileDiff
import vulpix.TestConfiguration
import vulpix.ParallelTesting
import reporting.TestReporter
import interfaces.Diagnostic.INFO
import dotty.tools.nio.*
import org.junit.Test

import java.io.{ByteArrayOutputStream, PrintStream}
import scala.io.Codec

class PrintingTest {

  def options(phase: String, flags: List[String]) =
    val outDir = ParallelTesting.defaultOutputDir.getOrCreateContainer("printing")
    List(s"-Vprint:$phase", "-color:never", "-nowarn", "-d", outDir.path, "-classpath", TestConfiguration.basicClasspath) ::: flags

  private def compileFile(file: File, phase: String): Boolean = {
    val baseFilePath  = file.path.stripSuffix(".scala").stripSuffix(".java")
    val checkFile = file.getOrCreateSiblingWithExtension(FileExtension.from("check"))
    val flagsFile = file.getSiblingWithExtension(FileExtension.from("flags"))
    val byteStream    = new ByteArrayOutputStream()
    val reporter = TestReporter.reporter(new PrintStream(byteStream), INFO)
    val flags = flagsFile match
      case Some(f) => f.readLines(Codec.UTF8).toList
      case None => Nil

    try {
      Main.process((file.path :: options(phase, flags)).toArray, reporter, null)
    } catch {
      case e: Throwable =>
        println(s"Compile ${file.path} exception:")
        e.printStackTrace()
        throw e
    }

    val actualLines = byteStream.toString(Codec.UTF8.charSet).linesIterator
    FileDiff.checkAndDumpOrUpdate(file.path, actualLines.toIndexedSeq, checkFile.path, tolerateMissingCheckFile = false)
  }

  def testIn(testsDir: String, phase: String) =
    val res = TestSources.rootPath().getContainer(testsDir).get.entries.collect {
      case f: File if f.extension.isSourceExtension => compileFile(f, phase)
    }.toList

    val failed = res.filter(!_)
    val msg = s"Pass: ${res.length - failed.length}, Failed: ${failed.length}"
    assert(failed.isEmpty, msg)
    println(msg)
  end testIn

  @Test
  def printing: Unit = testIn("tests/printing", "typer")

  @Test
  def posttyper: Unit = testIn("tests/printing/posttyper", "posttyper")

  @Test
  def untypedPrinting: Unit = testIn("tests/printing/untyped", "parser")

  @Test
  def transformedPrinting: Unit = testIn("tests/printing/transformed", "repeatableAnnotations")

  @Test
  def getters: Unit = testIn("tests/printing/getters", "getters")

  @Test
  def inlining: Unit = testIn("tests/printing/inlining", "inlining")
}
