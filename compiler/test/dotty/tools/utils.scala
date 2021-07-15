package dotty.tools

import java.io.File
import java.nio.charset.StandardCharsets.UTF_8

import scala.io.Source
import scala.reflect.ClassTag
import scala.util.Using.resource
import scala.util.chaining.given
import scala.util.control.{ControlThrowable, NonFatal}

def scripts(path: String): Array[File] = {
  val dir = new File(getClass.getResource(path).getPath)
  assert(dir.exists && dir.isDirectory, "Couldn't load scripts dir")
  dir.listFiles
}

extension (f: File) def absPath =
  f.getAbsolutePath.replace('\\', '/')

extension (str: String) def dropExtension =
  str.reverse.dropWhile(_ != '.').drop(1).reverse

private def withFile[T](file: File)(action: Source => T): T =
  resource(Source.fromFile(file, UTF_8.name))(action)

def readLines(f: File): List[String] = withFile(f)(_.getLines.toList)
def readFile(f: File): String = withFile(f)(_.mkString)

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
