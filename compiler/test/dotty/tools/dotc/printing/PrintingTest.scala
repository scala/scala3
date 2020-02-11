package dotty
package tools
package dotc

import vulpix.FileDiff
import vulpix.TestConfiguration
import vulpix.TestConfiguration
import reporting.TestReporter

import java.io._
import java.nio.file.{Files, Path => JPath, Paths}
import java.lang.System.{lineSeparator => EOL}

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

    val jOutFilePath = Paths.get(baseFilePath + ".check.out")
    if (Files.exists(jOutFilePath))
      try { Files.delete(jOutFilePath) } catch { case _: Exception => () }

    try {
      Main.process((path.toString::options).toArray, reporter, null)
    } catch {
      case e: Throwable =>
        println(s"Compile $path exception:")
        e.printStackTrace()
    }

    val actualLines = byteStream.toString("UTF-8").split("\\r?\\n")
    // 'options' includes option '-Xprint:typer' so the first output line
    // looks similar to "result of tests/printing/i620.scala after typer:";
    // check files use slashes as file separators (Unix) but running tests
    // on Windows produces backslashes.
    // NB. option '-Xprint:<..>' can specify several phases.
    val filteredLines =
      if (config.Properties.isWin)
        actualLines.map(line =>
          if (line.startsWith("result of")) line.replaceAll("\\\\", "/") else line
        )
      else
        actualLines

    FileDiff.checkAndDump(path.toString, filteredLines.toIndexedSeq, checkFilePath)
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
