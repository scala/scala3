package dotty.tools.dotc
package typer

import core._
import Phases._
import Contexts._
import Symbols._
import dotty.tools.dotc.parsing.JavaParsers.JavaParser
import parsing.Parsers.Parser
import config.Config
import config.Printers.{typr, default}
import util.Stats._
import scala.util.control.NonFatal
import ast.Trees._

class FrontEnd extends Phase {

  override def phaseName = "frontend"
  override def isTyper = true
  import ast.tpd

  /** The contexts for compilation units that are parsed but not yet entered */
  private[this] var remaining: List[Context] = Nil

  /** Does a source file ending with `<name>.scala` belong to a compilation unit
   *  that is parsed but not yet entered?
   */
  def stillToBeEntered(name: String): Boolean =
    remaining.exists(_.compilationUnit.toString.endsWith(name + ".scala"))

  def monitor(doing: String)(body: => Unit)(implicit ctx: Context) =
    try body
    catch {
      case NonFatal(ex) =>
        ctx.echo(s"exception occurred while $doing ${ctx.compilationUnit}")
        throw ex
    }

  def parse(implicit ctx: Context) = monitor("parsing") {
    val unit = ctx.compilationUnit
    unit.untpdTree =
      if (unit.isJava) new JavaParser(unit.source).parse()
      else new Parser(unit.source).parse()
    val printer = if (ctx.settings.Xprint.value.contains("parser")) default else typr
    printer.println("parsed:\n" + unit.untpdTree.show)
    if (Config.checkPositions)
      unit.untpdTree.checkPos(nonOverlapping = !unit.isJava && !ctx.reporter.hasErrors)
  }

  def enterSyms(implicit ctx: Context) = monitor("indexing") {
    val unit = ctx.compilationUnit
    ctx.typer.index(unit.untpdTree)
    typr.println("entered: " + unit.source)
  }

  def enterAnnotations(implicit ctx: Context) = monitor("annotating") {
    val unit = ctx.compilationUnit
    ctx.typer.annotate(unit.untpdTree :: Nil)
    typr.println("annotated: " + unit.source)
  }

  def typeCheck(implicit ctx: Context) = monitor("typechecking") {
    val unit = ctx.compilationUnit
    unit.tpdTree = ctx.typer.typedExpr(unit.untpdTree)
    typr.println("typed: " + unit.source)
    record("retained untyped trees", unit.untpdTree.treeSize)
    record("retained typed trees after typer", unit.tpdTree.treeSize)
  }

  private def firstTopLevelDef(trees: List[tpd.Tree])(implicit ctx: Context): Symbol = trees match {
    case PackageDef(_, defs) :: _    => firstTopLevelDef(defs)
    case Import(_, _) :: defs        => firstTopLevelDef(defs)
    case (tree @ TypeDef(_, _)) :: _ => tree.symbol
    case _ => NoSymbol
  }

  protected def discardAfterTyper(unit: CompilationUnit)(implicit ctx: Context) =
    unit.isJava || firstTopLevelDef(unit.tpdTree :: Nil).isPrimitiveValueClass

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
    val unitContexts = for (unit <- units) yield {
      ctx.inform(s"compiling ${unit.source}")
      ctx.fresh.setCompilationUnit(unit)
    }
    unitContexts foreach (parse(_))
    record("parsedTrees", ast.Trees.ntrees)
    remaining = unitContexts
    while (remaining.nonEmpty) {
      enterSyms(remaining.head)
      remaining = remaining.tail
    }
    unitContexts.foreach(enterAnnotations(_))
    unitContexts.foreach(typeCheck(_))
    record("total trees after typer", ast.Trees.ntrees)
    unitContexts.map(_.compilationUnit).filterNot(discardAfterTyper)
  }

  override def run(implicit ctx: Context): Unit = {
    parse
    enterSyms
    typeCheck
  }
}
