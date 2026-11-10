package dotty.tools
package dotc
package parsing

import dotty.tools.nio.*
import util.*
import scala.io.Codec
import Tokens.*, Parsers.*
import ast.untpd.*
import scala.collection.mutable.ListBuffer

class ParserTest extends DottyTest {

  def parse(name: String): Tree = parse(File.getOnDisk(name).get)

  var parsed = 0
  val parsedTrees = new ListBuffer[Tree]

  def reset() = {
    parsed = 0
    parsedTrees.clear()
  }

  def parse(file: File): Tree = parseSource(SourceFile(file, FileContainer.getOnDisk(".").get, Codec.UTF8))

  private def parseSource(source: SourceFile): Tree = {
    //println("***** parsing " + source.file)
    val parser = new Parser(source)
    val tree = parser.parse()
    parsed += 1
    parsedTrees += tree
    tree
  }

  def parseDir(path: String): Unit = parseDir(FileContainer.getOnDisk(path).get)

  def parseDir(dir: FileContainer): Unit = {
    for e <- dir.entries do
      e match
        case f: File => if f.extension.isScala then parse(f)
        case fc: FileContainer => parseDir(fc)
  }

  def parseText(code: String): Tree = parseSource(SourceFile.virtual("<code>", code))
}
