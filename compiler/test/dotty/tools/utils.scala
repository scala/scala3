package dotty
package tools

import scala.language.unsafeNulls

import java.io.File
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path => JPath}

import scala.io.Source
import scala.jdk.StreamConverters._
import scala.reflect.ClassTag
import scala.util.Using.resource
import scala.util.chaining.given
import scala.util.control.{ControlThrowable, NonFatal}

import dotc.config.CommandLineParser

object Dummy

def scripts(path: String): Array[File] = {
  val dir = new File(Dummy.getClass.getResource(path).getPath)
  assert(dir.exists && dir.isDirectory, "Couldn't load scripts dir")
  dir.listFiles.filter { f =>
    val path = if f.isDirectory then f.getPath + "/" else f.getPath
    Properties.testsFilter.isEmpty || Properties.testsFilter.exists(path.contains)
  }
}

extension (f: File) def absPath =
  f.getAbsolutePath.replace('\\', '/')

extension (str: String) def dropExtension =
  str.reverse.dropWhile(_ != '.').drop(1).reverse

private
def withFile[T](file: File)(action: Source => T): T = resource(Source.fromFile(file, UTF_8.name))(action)
def readLines(f: File): List[String]                = withFile(f)(_.getLines.toList)
def readFile(f: File): String                       = withFile(f)(_.mkString)

private object Unthrown extends ControlThrowable

def assertThrows[T <: Throwable: ClassTag](p: T => Boolean)(body: => Any): Unit =
  try
    body
    throw Unthrown
  catch
    case Unthrown        => throw AssertionError("Expression did not throw!")
    case e: T if p(e)    => ()
    case failed: T       => throw AssertionError(s"Exception failed check: $failed").tap(_.addSuppressed(failed))
    case NonFatal(other) => throw AssertionError(s"Wrong exception: expected ${implicitly[ClassTag[T]]} but was ${other.getClass.getName}").tap(_.addSuppressed(other))
end assertThrows

/** Famous tool names in the ecosystem. Used for tool args in test files. */
enum ToolName:
  case Scala, Scalac, Java, Javac, ScalaJS, Test
object ToolName:
  def named(s: String): ToolName = values.find(_.toString.equalsIgnoreCase(s)).getOrElse(throw IllegalArgumentException(s))

type ToolArgs = Map[ToolName, List[String]]

/** Take a prefix of each file, extract tool args, parse, and combine.
 *  Arg parsing respects quotation marks. Result is a map from ToolName to the combined tokens.
 */
def toolArgsFor(files: List[JPath], charset: Charset = UTF_8): ToolArgs =
  files.foldLeft(Map.empty[ToolName, List[String]]) { (res, path) =>
    val toolargs = toolArgsParse(resource(Files.lines(path, charset))(_.limit(10).toScala(List)), Some(path.toString))
    toolargs.foldLeft(res) {
      case (acc, (tool, args)) =>
        val name = ToolName.named(tool)
        val tokens = CommandLineParser.tokenize(args)
        acc.updatedWith(name)(v0 => v0.map(_ ++ tokens).orElse(Some(tokens)))
    }
  }

def toolArgsFor(tool: ToolName, filename: Option[String])(lines: List[String]): List[String] =
  toolArgsParse(lines, filename).collectFirst { case (name, args) if tool eq ToolName.named(name) => CommandLineParser.tokenize(args) }.getOrElse(Nil)

// scalajs: arg1 arg2, with alternative opening, optional space, alt names, text that is not */ up to end.
// groups are (name, args)
// note: ideally we would replace everything that requires this to use directive syntax, however scalajs: --skip has no directive equivalent yet.
private val toolArg = raw"(?://|/\*| \*) ?(?i:(${ToolName.values.mkString("|")})):((?:[^*]|\*(?!/))*)".r.unanchored
private val directiveOptionsArg = raw"//> using options (.*)".r.unanchored

// Inspect the lines for compiler options of the form
// `//> using options args`, `// scalajs: args`, `/* scalajs: args`, ` * scalajs: args` etc.
// If args string ends in close comment, stop at the `*` `/`.
// Returns all the matches by the regex.
def toolArgsParse(lines: List[String], filename: Option[String]): List[(String,String)] =
  lines.flatMap {
    case toolArg("scalac", _) => sys.error(s"`// scalac: args` not supported. Please use `//> using options args`${filename.fold("")(f => s" in file $f")}")
    case toolArg(name, args) => List((name, args))
    case _ => Nil
  } ++
  lines.flatMap { case directiveOptionsArg(args) => List(("scalac", args)) case _ => Nil }

import org.junit.Test
import org.junit.Assert._

class ToolArgsTest:
  @Test def `missing toolarg is absent`: Unit = assertEquals(Nil, toolArgsParse(List(""), None))
  @Test def `toolarg is present`: Unit = assertEquals(("test", " -hey") :: Nil, toolArgsParse("// test: -hey" :: Nil, None))
  @Test def `tool is present`: Unit = assertEquals("-hey" :: Nil, toolArgsFor(ToolName.Test, None)("// test: -hey" :: Nil))
  @Test def `missing tool is absent`: Unit = assertEquals(Nil, toolArgsFor(ToolName.Javac, None)("// test: -hey" :: Nil))
  @Test def `multitool is present`: Unit =
    assertEquals("-hey" :: Nil, toolArgsFor(ToolName.Test, None)("// test: -hey" :: "// javac: -d /tmp" :: Nil))
    assertEquals("-d" :: "/tmp" :: Nil, toolArgsFor(ToolName.Javac, None)("// test: -hey" :: "// javac: -d /tmp" :: Nil))
end ToolArgsTest
