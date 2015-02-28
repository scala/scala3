package dotty.tools.dotc
package typer

import core._
import Phases.Phase
import Contexts.Context
import config.Printers._
import util.Stats._

class FrontEnd extends Phase {

  override def phaseName = "frontend"
  
  override def stopOnError = false // always run FrontEnd, even if there are parse errors

  def enterSyms(implicit ctx: Context) = monitor("indexing") {
    val unit = ctx.compilationUnit
    ctx.typer.index(unit.untpdTree)
    typr.println("entered: "+unit.source)
  }

  def typeCheck(implicit ctx: Context) = monitor("typechecking") {
    val unit = ctx.compilationUnit
    unit.tpdTree = ctx.typer.typedExpr(unit.untpdTree)
    typr.println("typed: "+unit.source)
    record("retainedUntypedTrees", unit.untpdTree.treeSize)
    record("retainedTypedTrees", unit.tpdTree.treeSize)
  }

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
    val unitContexts = units map (unit => ctx.fresh.setCompilationUnit(unit))
    record("parsedTrees", ast.Trees.ntrees)
    unitContexts foreach (enterSyms(_))
    unitContexts foreach (typeCheck(_))
    record("totalTrees", ast.Trees.ntrees)
    unitContexts.map(_.compilationUnit)
  }

  override def run(implicit ctx: Context): Unit = {
    enterSyms
    typeCheck
  }
}