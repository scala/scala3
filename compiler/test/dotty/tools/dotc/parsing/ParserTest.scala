package dotty.tools
package dotc
package parsing

import dotty.tools.io._
import util._
import core._
import scala.io.Codec
import Tokens._, Parsers._
import ast.untpd._
import reporting.Diagnostic
import org.junit.Test
import scala.collection.mutable.ListBuffer

class ParserTest extends DottyTest {
  import ParserTest._

  def parse(name: String): Tree = parse(new PlainFile(File(name)))

  var parsed = 0
  val parsedTrees = new ListBuffer[Tree]

  def reset() = {
    parsed = 0
    parsedTrees.clear()
  }

  def reset(source: SourceFile) = {
    parsed = 0
    parsedTrees.clear()
    resetCtx(source)
  }

  def parse(file: PlainFile): Tree = parseSourceEither(new SourceFile(file, Codec.UTF8)).toTry.get

  private def parseSourceEither(source: SourceFile): Either[ParserError, Tree] = {
    //println("***** parsing " + source.file)
    reset(source)
    val parser = new Parser(source)
    val tree = parser.parse()
    if (getCtx.reporter.hasErrors) {
      val result = Left(ParserError(getCtx.reporter.allErrors))
      result
    }
    else {
      parsed += 1
      parsedTrees += tree
      Right(tree)
    }
  }

  def parseDir(path: String): Unit = parseDir(Directory(path))

  def parseDir(dir: Directory): Unit = {
    for (f <- dir.files)
      if (f.name.endsWith(".scala")) parse(new PlainFile(f))
    for (d <- dir.dirs)
      parseDir(d.path)
  }

  def parseText(code: String): Tree = parseTextEither(code).toTry.get

  def parseTextEither(code: String): Either[ParserError, Tree] =
    parseSourceEither(SourceFile.virtual("<code>", code))
}

object ParserTest {
  case class ParserError(errors: List[Diagnostic.Error]) extends RuntimeException
}
