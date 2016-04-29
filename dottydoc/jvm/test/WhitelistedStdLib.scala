package dotty.tools
package dottydoc

import scala.io.Source

object WhitelistedStdLib extends DottyDoc {
  override def main(args: Array[String]) = {
    val whitelist = "../../test/dotc/scala-collections.whitelist"
    val stdlibFiles = Source.fromFile(whitelist, "UTF8")
      .getLines()
      .map(_.trim) // allow identation
      .filter(!_.startsWith("#")) // allow comment lines prefixed by #
      .map(_.takeWhile(_ != '#').trim) // allow comments in the end of line
      .filter(_.nonEmpty)
      .filterNot(_.endsWith("package.scala"))
      .map("../." + _)
      .toArray

    super.main("-language:Scala2" +: stdlibFiles)
  }
}
