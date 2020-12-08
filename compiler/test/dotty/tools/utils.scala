package dotty.tools

import java.io.File
import java.nio.charset.StandardCharsets.UTF_8

import scala.io.Source
import scala.util.Using.resource

def scripts(path: String): Array[File] = {
  val dir = new File(getClass.getResource(path).getPath)
  assert(dir.exists && dir.isDirectory, "Couldn't load scripts dir")
  dir.listFiles
}

private def withFile[T](file: File)(action: Source => T): T =
  resource(Source.fromFile(file, UTF_8.name))(action)

def readLines(f: File): List[String] = withFile(f)(_.getLines.toList)
def readFile(f: File): String = withFile(f)(_.mkString)
