package dotty.tools.dotc
package typer

import core._
import Phases._
import Contexts._
import parsing.Parsers.Parser

class FrontEnd extends Phase {

  def name = "frontend"

  def parse(implicit ctx: Context) = {
    val unit = ctx.compilationUnit
    unit.untpdTree = new Parser(unit.source).parse()
    println("parsed:\n"+unit.untpdTree.show)
  }

  def enterSyms(implicit ctx: Context) = {
    val unit = ctx.compilationUnit
    ctx.typer.enterSym(unit.untpdTree)
    println("entered:\n"+unit.source)
  }

  def typeCheck(implicit ctx: Context) = {
    val unit = ctx.compilationUnit
    unit.tpdTree = ctx.typer.typedExpr(unit.untpdTree)
    println("typed:\n"+unit.source)
  }

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context): Unit = {
    val unitContexts = units map ctx.fresh.withCompilationUnit
    unitContexts foreach (parse(_))
    unitContexts foreach (enterSyms(_))
    unitContexts foreach (typeCheck(_))
  }

  override def run(implicit ctx: Context): Unit = {
    parse
    enterSyms
    typeCheck
  }
}