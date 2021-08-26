package dotty
package tools
package dotc
package transform

import vulpix.FileDiff
import vulpix.TestConfiguration
import reporting.TestReporter

import dotty.tools.io.Directory

import java.io._
import java.nio.file.{Files, Path => JPath}

import scala.io.Source._
import org.junit.Test

class PatmatExhaustivityTest {
  val testsDir = "tests/patmat"
  // pagewidth/color: for a stable diff as the defaults are based on the terminal (e.g size)
  // stop-after: patmatexhaust-huge.scala crash compiler (but also hides other warnings..)
  val options = List("-pagewidth", "80", "-color:never", "-Ystop-after:explicitSelf", "-classpath", TestConfiguration.basicClasspath)

  private def compile(files: List[JPath]): Seq[String] = {
    val opts         = toolArgsFor(files)
    val stringBuffer = new StringWriter()
    val printWriter  = new PrintWriter(stringBuffer)
    val reporter = TestReporter.simplifiedReporter(printWriter)

    try {
      Main.process((options ::: opts ::: files.map(_.toString)).toArray, reporter, null)
    } catch {
      case e: Throwable =>
        e.printStackTrace(printWriter)
    }

    stringBuffer.toString.trim.replaceAll("\\s+\n", "\n") match {
      case "" => Nil
      case s  => s.linesIterator.toSeq
    }
  }

  private def compileFile(path: JPath): Boolean = {
    val actualLines   = compile(List(path))
    val baseFilePath  = path.toString.stripSuffix(".scala")
    val checkFilePath = baseFilePath + ".check"

    FileDiff.checkAndDumpOrUpdate(path.toString, actualLines, checkFilePath)
  }

  /** A single test with multiple files grouped in a folder */
  private def compileDir(path: JPath): Boolean = {
    val files = Directory(path).list.toList
      .filter(f => f.extension == "scala" || f.extension == "java" )
      .map(_.jpath)

    val actualLines   = compile(files)
    val checkFilePath = s"${path}${File.separator}expected.check"

    FileDiff.checkAndDumpOrUpdate(path.toString, actualLines, checkFilePath)
  }

  @Test
  def patmatExhaustivity: Unit = {
    val res = Directory(testsDir).list.toList
      .filter(f => f.extension == "scala" || f.isDirectory)
      .filter { f =>
        val path = if f.isDirectory then f.path + "/" else f.path
        path.contains(Properties.testsFilter.getOrElse(""))
      }
      .map(f => if f.isDirectory then compileDir(f.jpath) else compileFile(f.jpath))

    val failed = res.filter(!_)
    val ignored = Directory(testsDir).list.toList.filter(_.extension == "ignore")

    val msg = s"Total: ${res.length + ignored.length}, Failed: ${failed.length}, Ignored: ${ignored.length}"

    assert(failed.length == 0, msg)

    println(msg)
  }

  // inspect given files for tool args of the form `tool: args`
  // if args string ends in close comment, drop the `*` `/`
  // if split, parse the args string as command line.
  // (from scala.tools.partest.nest.Runner#toolArgsFor)
  private def toolArgsFor(files: List[JPath]): List[String] = {
    import scala.jdk.OptionConverters._
    import config.CommandLineParser.tokenize
    files.flatMap { path =>
      val tag  = "scalac:"
      val endc = "*" + "/"    // be forgiving of /* scalac: ... */
      def stripped(s: String) = s.substring(s.indexOf(tag) + tag.length).stripSuffix(endc)
      val args = scala.util.Using.resource(Files.lines(path, scala.io.Codec.UTF8.charSet))(
        _.limit(10).filter(_.contains(tag)).map(stripped).findAny.toScala
      )
      args.map(tokenize).getOrElse(Nil)
    }
  }
}
