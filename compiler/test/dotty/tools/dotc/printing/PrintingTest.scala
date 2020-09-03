package dotty
package tools
package dotc

import vulpix.FileDiff
import vulpix.TestConfiguration
import vulpix.TestConfiguration
import reporting.TestReporter

import java.io._
import java.nio.file.{Path => JPath}
import java.lang.System.{lineSeparator => EOL}
import java.nio.charset.StandardCharsets

import interfaces.Diagnostic.INFO
import dotty.tools.io.Directory

import scala.io.Source
import org.junit.Test

class PrintingTest {
  val testsDir = "tests/printing"
  val options = List("-Xprint:typer", "-color:never", "-classpath", TestConfiguration.basicClasspath)

  private def compileFile(path: JPath): Boolean = {
    val baseFilePath  = path.toString.stripSuffix(".scala")
    val checkFilePath = baseFilePath + ".check"
    val byteStream    = new ByteArrayOutputStream()
    val reporter = TestReporter.reporter(new PrintStream(byteStream), INFO)

    try {
      Main.process((path.toString::options).toArray, reporter, null)
    } catch {
      case e: Throwable =>
        println(s"Compile $path exception:")
        e.printStackTrace()
    }

    val actualLines = byteStream.toString(StandardCharsets.UTF_8.name).linesIterator
    FileDiff.checkAndDump(path.toString, actualLines.toIndexedSeq, checkFilePath)
  }

  @Test
  def printing: Unit = {
    val res = Directory(testsDir).list.toList
      .filter(f => f.extension == "scala")
      .map { f => compileFile(f.jpath) }

    val failed = res.filter(!_)

    val msg = s"Pass: ${res.length - failed.length}, Failed: ${failed.length}"

    assert(failed.length == 0, msg)

    println(msg)
  }
}
