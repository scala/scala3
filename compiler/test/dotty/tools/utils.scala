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

def scripts(path: String): Array[File] = {
  val dir = new File(getClass.getResource(path).getPath)
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

def toolArgsFor(files: List[JPath], charset: Charset = UTF_8): List[String] =
  files.flatMap(path => toolArgsParse(resource(Files.lines(path, charset))(_.limit(10).toScala(List))))

// Inspect the first 10 of the given lines for compiler options of the form
// `// scalac: args`, `/* scalac: args`, ` * scalac: args`.
// If args string ends in close comment, drop the `*` `/`.
// If split, parse the args string as a command line.
// (from scala.tools.partest.nest.Runner#toolArgsFor)
def toolArgsParse(lines: List[String]): List[String] = {
  val tag  = "scalac:"
  val endc = "*" + "/"    // be forgiving of /* scalac: ... */
  def stripped(s: String) = s.substring(s.indexOf(tag) + tag.length).stripSuffix(endc)
  val args = lines.to(LazyList).take(10).filter { s =>
       s.contains("//" + tag)
    || s.contains("// " + tag)
    || s.contains("/* " + tag)
    || s.contains(" * " + tag)
    // but avoid picking up comments like "% scalac ./a.scala" and "$ scalac a.scala"
  }.map(stripped).headOption
  args.map(dotc.config.CommandLineParser.tokenize).getOrElse(Nil)
}