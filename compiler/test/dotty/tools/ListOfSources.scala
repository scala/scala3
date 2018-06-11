package dotty.tools

import scala.io.Source

object ListOfSources {
  def loadList(path: String): List[String] = Source.fromFile(path, "UTF8").getLines()
    .map(_.trim) // allow identation
    .filter(!_.startsWith("#")) // allow comment lines prefixed by #
    .map(_.takeWhile(_ != '#').trim) // allow comments in the end of line
    .filter(_.nonEmpty)
    .toList
}