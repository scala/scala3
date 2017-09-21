package dotty
package tools
package dotc
package transform

import java.io._

import scala.io.Source._
import dotty.tools.io.Directory
import org.junit.Test
import reporting.TestReporter
import vulpix.TestConfiguration

class PatmatExhaustivityTest {
  val testsDir = "../tests/patmat"
  // stop-after: patmatexhaust-huge.scala crash compiler
  val options = List("-color:never", "-Ystop-after:splitter", "-Ycheck-all-patmat", "-classpath", TestConfiguration.classPath)

  private def compileFile(file: File) = {
    val stringBuffer = new StringWriter()
    val reporter = TestReporter.simplifiedReporter(new PrintWriter(stringBuffer))

    try {
      Main.process((file.getPath::options).toArray, reporter, null)
    } catch {
      case e: Throwable =>
        println(s"Compile $file exception:")
        e.printStackTrace()
    }

    val actual = stringBuffer.toString.trim.replaceAll("\\s+\n", "\n")
    val checkFilePath = file.getAbsolutePath.stripSuffix(".scala") + ".check"
    val checkContent =
      if (new File(checkFilePath).exists)
        fromFile(checkFilePath).getLines().map(_.replaceAll("\\s+$", "")).mkString("\n").trim
      else ""

    (file, checkContent, actual)
  }

  /** A single test with multiple files grouped in a folder */
  private def compileDir(file: File) = {
    val stringBuffer = new StringWriter()
    val reporter = TestReporter.simplifiedReporter(new PrintWriter(stringBuffer))

    val files = Directory(file.getPath).list.toList
      .filter(f => f.extension == "scala" || f.extension == "java" )
      .map(_.jfile.getPath)

    try {
      Main.process((options ++ files).toArray, reporter, null)
    } catch {
      case e: Throwable =>
        println(s"Compile $file exception:")
        e.printStackTrace()
    }

    val actual = stringBuffer.toString.trim.replaceAll("\\s+\n", "\n")
    val checkFilePath = file.getPath + File.separator + "expected.check"
    val checkContent =
      if (new File(checkFilePath).exists)
        fromFile(checkFilePath).getLines().map(_.replaceAll("\\s+$", "")).mkString("\n").trim
      else ""

    (file, checkContent, actual)
  }

  @Test
  def patmatExhaustivity: Unit = {
    val res = Directory(testsDir).list.toList
      .filter(f => f.extension == "scala" || f.isDirectory)
      .map { f =>
        if (f.isDirectory)
          compileDir(f.jfile)
        else
          compileFile(f.jfile)
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
