package dotty
package tools
package dotc
package transform

import java.io._
import java.nio.file.{Path => JPath}

import scala.io.Source._
import dotty.tools.io.Directory
import org.junit.Test
import reporting.TestReporter
import vulpix.TestConfiguration

class PatmatExhaustivityTest {
  val testsDir = "../tests/patmat"
  // stop-after: patmatexhaust-huge.scala crash compiler
  val options = List("-color:never", "-Ystop-after:splitter", "-Ycheck-all-patmat", "-classpath", TestConfiguration.classPath)

  private def compileFile(path: JPath) = {
    val stringBuffer = new StringWriter()
    val reporter = TestReporter.simplifiedReporter(new PrintWriter(stringBuffer))

    try {
      Main.process((path.toString::options).toArray, reporter, null)
    } catch {
      case e: Throwable =>
        println(s"Compile $path exception:")
        e.printStackTrace()
    }

    val actual = stringBuffer.toString.trim.replaceAll("\\s+\n", "\n")
    val checkFilePath = path.toAbsolutePath.toString.stripSuffix(".scala") + ".check"
    val checkContent =
      if (new File(checkFilePath).exists)
        fromFile(checkFilePath).getLines().map(_.replaceAll("\\s+$", "")).mkString("\n").trim
      else ""

    (path, checkContent, actual)
  }

  /** A single test with multiple files grouped in a folder */
  private def compileDir(path: JPath) = {
    val stringBuffer = new StringWriter()
    val reporter = TestReporter.simplifiedReporter(new PrintWriter(stringBuffer))

    val files = Directory(path).list.toList
      .filter(f => f.extension == "scala" || f.extension == "java" )
      .map(_.jpath.toString)

    try {
      Main.process((options ++ files).toArray, reporter, null)
    } catch {
      case e: Throwable =>
        println(s"Compile $path exception:")
        e.printStackTrace()
    }

    val actual = stringBuffer.toString.trim.replaceAll("\\s+\n", "\n")
    val checkFilePath = path + File.separator + "expected.check"
    val checkContent =
      if (new File(checkFilePath).exists)
        fromFile(checkFilePath).getLines().map(_.replaceAll("\\s+$", "")).mkString("\n").trim
      else ""

    (path, checkContent, actual)
  }

  @Test
  def patmatExhaustivity: Unit = {
    val res = Directory(testsDir).list.toList
      .filter(f => f.extension == "scala" || f.isDirectory)
      .map { f =>
        if (f.isDirectory)
          compileDir(f.jpath)
        else
          compileFile(f.jpath)
      }

    val failed = res.filter { case (_, expected, actual) => expected != actual }
    val ignored = Directory(testsDir).list.toList.filter(_.extension == "ignore")

    failed.foreach { case (file, expected, actual) =>
      println(s"\n----------------- incorrect output for $file --------------\n" +
        s"Expected:\n---------\n$expected\n\nActual:\n-------\n$actual\n"
      )
    }

    val msg = s"Total: ${res.length + ignored.length}, Failed: ${failed.length}, Ignored: ${ignored.length}"

    assert(failed.length == 0, msg)

    println(msg)
  }
}
