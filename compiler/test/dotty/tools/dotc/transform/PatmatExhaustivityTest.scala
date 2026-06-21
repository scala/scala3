package dotty
package tools
package dotc
package transform

import scala.language.unsafeNulls

import vulpix.FileDiff
import vulpix.TestConfiguration
import reporting.TestReporter

import dotty.tools.io.Directory

import java.io.*
import java.nio.file.{Path => JPath}

import org.junit.Test

class PatmatExhaustivityTest {
  val testsDir = "tests/patmat"
  // pagewidth/color: for a stable diff as the defaults are based on the terminal (e.g size)
  // stop-after: patmatexhaust-huge.scala crash compiler (but also hides other warnings..)
  val options = Vector("-pagewidth", "80", "-color:never", "-Ystop-after:explicitSelf", "-Ycheck-constraint-deps", "-classpath", TestConfiguration.basicClasspath)

  private def compile(files: Vector[JPath]): Seq[String] = {
    val opts         = toolArgsFor(files).get(ToolName.Scalac).getOrElse(Vector())
    val stringBuffer = new StringWriter()
    val printWriter  = new PrintWriter(stringBuffer)
    val reporter = TestReporter.simplifiedReporter(printWriter)

    try {
      Main.process((options ++ opts ++ files.map(_.toString)).toArray, reporter, null)
    } catch {
      case e: Throwable =>
        e.printStackTrace(printWriter)
    }

    stringBuffer.toString.trim.replaceAll("\\s+\n", "\n") match {
      case "" => Vector()
      case s  => s.linesIterator.toSeq
    }
  }

  private def compileFile(path: JPath): Boolean = {
    val actualLines   = compile(Vector(path))
    val baseFilePath  = path.toString.stripSuffix(".scala")
    val checkFilePath = baseFilePath + ".check"

    FileDiff.checkAndDumpOrUpdate(path.toString, actualLines, checkFilePath)
  }

  /** A single test with multiple files grouped in a folder */
  private def compileDir(path: JPath): Boolean = {
    val files = Directory(path).list.toVector
      .filter(_.ext.isSourceExtension)
      .map(_.jpath)

    val actualLines   = compile(files)
    val checkFilePath = s"${path}${File.separator}expected.check"

    FileDiff.checkAndDumpOrUpdate(path.toString, actualLines, checkFilePath)
  }

  @Test
  def patmatExhaustivity: Unit = {
    val excluded = TestSources.patmatExhaustivityScala2LibraryTastyExcludelisted.toSet
    val res = Directory(testsDir).list.toVector
      .filter(f => f.ext.isScala || f.isDirectory)
      .filter { f =>
        val path = if f.isDirectory then f.path + "/" else f.path
        Properties.testsFilter.isEmpty || Properties.testsFilter.exists(path.contains)
      }
      .filterNot(f => excluded.contains(f.name))
      .map(f => if f.isDirectory then compileDir(f.jpath) else compileFile(f.jpath))

    val failed = res.filter(!_)
    val ignored = Directory(testsDir).list.toVector.filter(_.ext.toLowerCase.equalsIgnoreCase("ignore"))

    val msg = s"Total: ${res.length + ignored.length}, Failed: ${failed.length}, Ignored: ${ignored.length}"

    assert(failed.length == 0, msg)

    println(msg)
  }
}
