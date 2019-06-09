package dotty
package tools
package dotc

import vulpix.TestConfiguration
import vulpix.TestConfiguration
import reporting.TestReporter

import java.io._
import java.nio.file.{Path => JPath}
import java.lang.System.{lineSeparator => EOL}

import interfaces.Diagnostic.INFO
import dotty.tools.io.Directory

import scala.io.Source
import org.junit.Test

class PrintingTest {
  val testsDir = "tests/printing"
  val options = List("-Xprint:frontend", "-color:never", "-classpath", TestConfiguration.basicClasspath)

  private def fileContent(filePath: String): List[String] =
    if (new File(filePath).exists)
      Source.fromFile(filePath, "UTF-8").getLines().toList
    else Nil


  private def compileFile(path: JPath): Boolean = {
    val baseFilePath  = path.toAbsolutePath.toString.stripSuffix(".scala")
    val outFilePath   = baseFilePath + ".out"
    val checkFilePath = baseFilePath + ".check"
    val ps = new PrintStream(new File(outFilePath))
    val reporter = TestReporter.reporter(ps, INFO)

    try {
      Main.process((path.toString::options).toArray, reporter, null)
    } catch {
      case e: Throwable =>
        println(s"Compile $path exception:")
        e.printStackTrace()
    }
    finally ps.close()

    val actual   = fileContent(outFilePath)
    val expected = fileContent(checkFilePath)

    val success =
      actual.length == expected.length &&
      (actual, expected).zipped.forall(_ == _)

    success || {
      println(
        s"""|Output from '$outFilePath' did not match check file. Actual output:
            |${actual.mkString(EOL)}
            |""".stripMargin + "\n")

      println(
          s"""Test output dumped in: ${outFilePath}
             |  See diff of the checkfile
             |    > diff $checkFilePath $outFilePath
             |  Replace checkfile with current output output
             |    > mv $outFilePath $checkFilePath
         """.stripMargin)

      false
    }
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
