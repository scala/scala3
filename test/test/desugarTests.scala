package test

import scala.reflect.io._
import dotty.tools.dotc.util._
import dotty.tools.dotc.core._
import dotty.tools.dotc.parsing._
import Tokens._, Parsers._
import dotty.tools.dotc.ast.untpd._
import org.junit.Test
import scala.collection.mutable.ListBuffer

class desugarTests extends DeSugarTest {

  @Test
  def parseList(): Unit = {
    println(System.getProperty("user.dir"))
    parse("src/dotty/tools/dotc/core/Symbols.scala")
    parse("src/dotty/tools/dotc/core/Types.scala")
    desugarAll()
    reset()
  }

  @Test
  def parseDotty(): Unit = {
    parseDir("src")
    desugarAll()
    reset()
  }

  @Test
  def parseScala() = {
    parseDir("/Users/odersky/workspace/scala/src")
    desugarAll()
    reset()
  }
}