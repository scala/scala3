package dotty.tools
package dottydoc

import scala.io.Source

object WhitelistedStandardLib extends DottyDoc {
  val files: List[String] = {
    val whitelist = "../../test/dotc/scala-collections.whitelist"

    Source.fromFile(whitelist, "UTF8")
      .getLines()
      .map(_.trim) // allow identation
      .filter(!_.startsWith("#")) // allow comment lines prefixed by #
      .map(_.takeWhile(_ != '#').trim) // allow comments in the end of line
      .filter(_.nonEmpty)
      .filterNot(_.endsWith("package.scala"))
      .map("../." + _)
      .toList
  }

  override def main(args: Array[String]) =
    super.main("-language:Scala2" +: files.toArray)
}
