package dotty.tools.dotc
package parsing

import core._
import Phases.Phase
import Contexts.Context
import dotty.tools.dotc.parsing.JavaParsers.JavaParser
import parsing.Parsers.Parser

class Parsing extends Phase {

  override def phaseName = "parsing"
  
  override def untypedResult = true

  override def run(implicit ctx: Context): Unit = monitor("parsing") {
    val unit = ctx.compilationUnit
    unit.untpdTree =
      if (unit.isJava) new JavaParser(unit.source).parse()
      else new Parser(unit.source).parse()
  }
}