package dotty.tools.dotc
package typer

import core._
import Phases._
import Contexts._
import dotty.tools.dotc.parsing.JavaParsers.JavaParser
import parsing.Parsers.Parser
import config.Printers._
import util.Stats._
import scala.util.control.NonFatal

class FrontEnd extends Phase {

  override def phaseName = "frontend"

  def monitor(doing: String)(body: => Unit)(implicit ctx: Context) =
    try body
    catch {
      case NonFatal(ex) =>
        println(s"exception occured while $doing ${ctx.compilationUnit}")
        throw ex
    }

  def parse(implicit ctx: Context) = monitor("parsing") {
    val unit = ctx.compilationUnit
    unit.untpdTree =
      if (unit.isJava) new JavaParser(unit.source).parse()
      else new Parser(unit.source).parse()
    typr.println("parsed:\n" + unit.untpdTree.show)
  }

  def enterSyms(implicit ctx: Context) = monitor("indexing") {
    val unit = ctx.compilationUnit
    ctx.typer.index(unit.untpdTree)
    typr.println("entered: " + unit.source)
  }

  def typeCheck(implicit ctx: Context) = monitor("typechecking") {
    val unit = ctx.compilationUnit
    unit.tpdTree = ctx.typer.typedExpr(unit.untpdTree)
    typr.println("typed: " + unit.source)
    record("retainedUntypedTrees", unit.untpdTree.treeSize)
    record("retainedTypedTrees", unit.tpdTree.treeSize)
  }

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
    val unitContexts = units map (unit => ctx.fresh.setCompilationUnit(unit))
    unitContexts foreach (parse(_))
    record("parsedTrees", ast.Trees.ntrees)
    unitContexts foreach (enterSyms(_))
    unitContexts foreach (typeCheck(_))
    record("totalTrees", ast.Trees.ntrees)
    unitContexts.map(_.compilationUnit).filter(!_.isJava)
  }

  override def run(implicit ctx: Context): Unit = {
    parse
    enterSyms
    typeCheck
  }
}
