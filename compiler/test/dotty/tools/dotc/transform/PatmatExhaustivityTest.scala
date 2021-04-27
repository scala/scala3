package dotty
package tools
package dotc
package transform

import vulpix.FileDiff
import vulpix.TestConfiguration
import reporting.TestReporter

import dotty.tools.io.Directory

import java.io._
import java.nio.file.{Path => JPath}

import scala.io.Source._
import org.junit.Test

class PatmatExhaustivityTest {
  val testsDir = "tests/patmat"
  // stop-after: patmatexhaust-huge.scala crash compiler
  val options = List("-pagewidth", "80", "-color:never", "-Ystop-after:explicitSelf", "-Ycheck-all-patmat", "-classpath", TestConfiguration.basicClasspath)

  private def compile(files: Seq[String]): Seq[String] = {
    val stringBuffer = new StringWriter()
    val reporter = TestReporter.simplifiedReporter(new PrintWriter(stringBuffer))

    try {
      Main.process((options ++ files).toArray, reporter, null)
    } catch {
      case e: Throwable =>
        println(s"Compile $files exception:")
        e.printStackTrace()
    }

    stringBuffer.toString.trim.replaceAll("\\s+\n", "\n") match {
      case "" => Nil
      case s  => s.linesIterator.toSeq
    }
  }

  private def compileFile(path: JPath): Boolean = {
    val actualLines   = compile(path.toString :: Nil)
    val baseFilePath  = path.toString.stripSuffix(".scala")
    val checkFilePath = baseFilePath + ".check"

    FileDiff.checkAndDump(path.toString, actualLines, checkFilePath)
  }

  /** A single test with multiple files grouped in a folder */
  private def compileDir(path: JPath): Boolean = {
    val files = Directory(path).list.toList
      .filter(f => f.extension == "scala" || f.extension == "java" )
      .map(_.jpath.toString)

    val actualLines   = compile(files)
    val checkFilePath = s"${path}${File.separator}expected.check"

    FileDiff.checkAndDump(path.toString, actualLines, checkFilePath)
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

    val failed = res.filter(!_)
    val ignored = Directory(testsDir).list.toList.filter(_.extension == "ignore")

    val msg = s"Total: ${res.length + ignored.length}, Failed: ${failed.length}, Ignored: ${ignored.length}"

    assert(failed.length == 0, msg)

    println(msg)
  }
}
