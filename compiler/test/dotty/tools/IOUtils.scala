package dotty.tools

import java.nio.file.{FileSystems, FileVisitOption, FileVisitResult, FileVisitor, Files, Path}
import java.nio.file.attribute.BasicFileAttributes
import java.util.EnumSet

import scala.collection.JavaConverters.asScalaIteratorConverter

object IOUtils {

  def inTempDirectory[T](fn: Path => T): T = {
    val temp = Files.createTempDirectory("temp")
    try fn(temp)
    finally {
      val allFiles = getAll(temp, "glob:**").sortBy(_.toAbsolutePath.toString).reverse
      allFiles.foreach(Files.delete(_))
    }
  }

  def getAll(base: Path, pattern: String): List[Path] = {
    val paths = Files.walk(base)
    val matcher = FileSystems.getDefault.getPathMatcher(pattern)
    try paths.filter(matcher.matches).iterator().asScala.toList
    finally paths.close()
  }

}
