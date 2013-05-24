package test

import scala.reflect.io._
import dotty.tools.dotc.util._
import dotty.tools.dotc.core._
import dotty.tools.dotc.parsing._
import Tokens._, Parsers._
import dotty.tools.dotc.ast.untpd._
import org.junit.Test
import scala.collection.mutable.ListBuffer

class parserTests extends ParserTest {

  @Test
  def parseList(): Unit = {
    println(System.getProperty("user.dir"))
    parse("src/dotty/tools/dotc/core/Symbols.scala")
    parse("src/dotty/tools/dotc/core/Types.scala")
    reset()
  }

  @Test
  def parseDotty(): Unit = {
    parseDir("src")
    reset()
  }

  @Test
  def parseScala() = {
    parseDir("/Users/odersky/workspace/scala/src")
    reset()
  }

}