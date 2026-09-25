package dotty
package tools

import scala.reflect.ClassTag
import scala.util.Using.{Releasable, resource}
import scala.util.chaining.given
import scala.util.control.{ControlThrowable, NonFatal}
import dotc.config.CommandLineParser
import dotty.tools.directives.{DirectiveValue, UsingDirectivesParser}
import dotty.tools.nio.*
import scala.io.Codec

private object Dummy

def scripts(path: String): Array[FileSystemEntry] = {
  val dir = scriptsDir(path)
  dir.entries.filter { f =>
    val path = if f.isInstanceOf[FileContainer] then f.path + "/" else f.path
    Properties.testsFilter.isEmpty || Properties.testsFilter.exists(path.contains)
  }.toArray
}

def scriptsDir(path: String): FileContainer =
  FileContainer.getOnDisk(Dummy.getClass.getResource(path).getPath).getOrElse(throw new AssertionError("Couldn't load scripts dir"))

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

enum TestPlatform:
  case JVM, ScalaJS
  override def toString: String = this match
    case JVM     => "jvm"
    case ScalaJS => "scala-js"

object TestPlatform:
  def named(s: String): TestPlatform = s match
    case "jvm"      => TestPlatform.JVM
    case "scala-js" => TestPlatform.ScalaJS
    case _          => throw IllegalArgumentException(s)

/** Famous tool names in the ecosystem. Used for tool args in test files. */
enum ToolName:
  case Scala, Scalac, Java, Javac, ScalaJS, Test, Target
object ToolName:
  def named(s: String): ToolName = values.find(_.toString.equalsIgnoreCase(s)).getOrElse(throw IllegalArgumentException(s))

type ToolArgs = Map[ToolName, List[String]]
type PlatformFiles = Map[TestPlatform, List[String]]

/** Take a prefix of each file, extract tool args, parse, and combine.
 *  Arg parsing respects quotation marks. Result is a map from ToolName to the combined tokens.
 */
def toolArgsFor(files: List[File], codec: Codec = Codec.UTF8): ToolArgs =
  val (_, toolArgs) = platformAndToolArgsFor(files, codec)
  toolArgs

/** Take a prefix of each file, extract tool args, parse, and combine.
 *  Arg parsing respects quotation marks. Result is a map from ToolName to the combined tokens.
 *  If the ToolName is Target, then also accumulate the file name associated with the given platform.
 */
def platformAndToolArgsFor(files: List[File], codec: Codec = Codec.UTF8): (PlatformFiles, ToolArgs) =
  files.foldLeft(Map.empty[TestPlatform, List[String]] -> Map.empty[ToolName, List[String]]) { (res, path) =>
    val lines = path.readLines(codec).take(10).toList
    val content = path.readText(codec)
    val toolargs = toolArgsParse(lines, content, Some(path.path))
    toolargs.foldLeft(res) {
      case ((plat, acc), (tool, args)) =>
        val name = ToolName.named(tool)
        val tokens = CommandLineParser.tokenize(args)

        val plat1 = if name eq ToolName.Target then
          val testPlatform = TestPlatform.named(tokens.head)
          val fileName = path.path
          plat.updatedWith(testPlatform)(_.map(fileName :: _).orElse(Some(fileName :: Nil)))
        else
          plat

        plat1 -> acc.updatedWith(name)(v0 => v0.map(_ ++ tokens).orElse(Some(tokens)))
    }
  }

def toolArgsFor(tool: ToolName, filename: Option[String])(lines: List[String]): List[String] =
  toolArgsParse(lines, lines.mkString("\n"), filename).collectFirst { case (name, args) if tool eq ToolName.named(name) => CommandLineParser.tokenize(args) }.getOrElse(Nil)

// scalajs: arg1 arg2, with alternative opening, optional space, alt names, text that is not */ up to end.
// groups are (name, args)
// note: ideally we would replace everything that requires this to use directive syntax, however scalajs: --skip has no directive equivalent yet.
private val toolArg = raw"(?://|/\*| \*) ?(?i:(${ToolName.values.mkString("|")})):((?:[^*]|\*(?!/))*)".r.unanchored

// ================================================================================================
// =================================== VULPIX DIRECTIVES ==========================================
// ================================================================================================

private def directiveValuesAsArgs(values: Seq[DirectiveValue]): String =
  values.map(_.rawText).mkString(" ")

private def directiveToolArgs(content: String, filename: Option[String]): List[(String, String)] =
  UsingDirectivesParser.parse(content).directives.flatMap: directive =>
    directive.key match
      case "option" | "options" =>
        List(("scalac", directiveValuesAsArgs(directive.values)))
      case "javacOpt" =>
        List(("javac", directiveValuesAsArgs(directive.values)))
      case "target.platform" =>
        directive.values.headOption.map(v => ("target", v.stringValue)).toList
      case "scala" =>
        Nil
      case key =>
        sys.error(s"Unknown directive: `//> using $key`${filename.fold("")(f => s" in file $f")}")
  .toList

// Inspect the lines for compiler options of the form
// `//> using options args`, `// scalajs: args`, `/* scalajs: args`, ` * scalajs: args` etc.
// If args string ends in close comment, stop at the `*` `/`.
// Returns all the matches by the regex.
def toolArgsParse(lines: List[String], content: String, filename: Option[String]): List[(String,String)] =
  lines.flatMap {
    case toolArg("scalac", _) => sys.error(s"`// scalac: args` not supported. Please use `//> using options args`${filename.fold("")(f => s" in file $f")}")
    case toolArg("javac", _) => sys.error(s"`// javac: args` not supported. Please use `//> using javacOpt args`${filename.fold("")(f => s" in file $f")}")
    case toolArg(name, args) => List((name.nn, args.nn))
    case _ => Nil
  } ++ directiveToolArgs(content, filename)

import org.junit.Test
import org.junit.Assert.*

class ToolArgsTest:
  @Test def `missing toolarg is absent`: Unit = assertEquals(Nil, toolArgsParse(List(""), "", None))
  @Test def `toolarg is present`: Unit = assertEquals(("test", " -hey") :: Nil, toolArgsParse("// test: -hey" :: Nil, "// test: -hey", None))
  @Test def `tool is present`: Unit = assertEquals("-hey" :: Nil, toolArgsFor(ToolName.Test, None)("// test: -hey" :: Nil))
  @Test def `missing tool is absent`: Unit = assertEquals(Nil, toolArgsFor(ToolName.Javac, None)("// test: -hey" :: Nil))
  @Test def `multitool is present`: Unit =
    assertEquals("-hey" :: Nil, toolArgsFor(ToolName.Test, None)("// test: -hey" :: "// java: -d /tmp" :: Nil))
    assertEquals("-d" :: "/tmp" :: Nil, toolArgsFor(ToolName.Java, None)("// test: -hey" :: "// java: -d /tmp" :: Nil))
end ToolArgsTest
