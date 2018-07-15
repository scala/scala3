package dotty.tools

import java.io.File
import scala.io.Source
import org.junit.Test
import org.junit.Assert._

object StdLibSources {

  private final val stdLibPath = "scala2-library/src/library/"

  def blacklistFile: String = "compiler/test/dotc/scala-collections.blacklist"

  def blacklisted: List[String] = loadList(blacklistFile)

  private def loadList(path: String): List[String] = Source.fromFile(path, "UTF8").getLines()
    .map(_.trim) // allow identation
    .filter(!_.startsWith("#")) // allow comment lines prefixed by #
    .map(_.takeWhile(_ != '#').trim) // allow comments in the end of line
    .filter(_.nonEmpty)
    .map(stdLibPath + _)
    .toList

}
