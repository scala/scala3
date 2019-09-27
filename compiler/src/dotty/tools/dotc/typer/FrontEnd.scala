package dotty.tools
package dotc
package typer

import core._
import Phases._
import Contexts._
import Symbols._
import Decorators._
import dotty.tools.dotc.parsing.JavaParsers.JavaParser
import parsing.Parsers.Parser
import config.Config
import config.Printers.{typr, default}
import util.Stats._
import util.{ SourcePosition, NoSourcePosition }
import scala.util.control.NonFatal
import ast.Trees._

class FrontEnd extends Phase {

  override def phaseName: String = FrontEnd.name
  override def isTyper: Boolean = true
  import ast.tpd

  override def allowsImplicitSearch: Boolean = true

  /** The contexts for compilation units that are parsed but not yet entered */
  private var remaining: List[Context] = Nil

  /** The position of the first XML literal encountered while parsing,
   *  NoSourcePosition if there were no XML literals.
   */
  private var firstXmlPos: SourcePosition = NoSourcePosition

  /** Does a source file ending with `<name>.scala` belong to a compilation unit
   *  that is parsed but not yet entered?
   */
  def stillToBeEntered(name: String): Boolean =
    remaining.exists(_.compilationUnit.toString.endsWith(name + ".scala"))

  def monitor(doing: String)(body: => Unit)(implicit ctx: Context): Unit =
    try body
    catch {
      case NonFatal(ex) =>
        ctx.echo(s"exception occurred while $doing ${ctx.compilationUnit}")
        throw ex
    }

  def parse(implicit ctx: Context): Unit = monitor("parsing") {
    val unit = ctx.compilationUnit

    unit.untpdTree =
      if (unit.isJava) new JavaParser(unit.source).parse()
      else {
        val p = new Parser(unit.source)
        val tree = p.parse()
        if (p.firstXmlPos.exists && !firstXmlPos.exists)
          firstXmlPos = p.firstXmlPos
        tree
      }

    val printer = if (ctx.settings.Xprint.value.contains("parser")) default else typr
    printer.println("parsed:\n" + unit.untpdTree.show)
    if (Config.checkPositions)
      unit.untpdTree.checkPos(nonOverlapping = !unit.isJava && !ctx.reporter.hasErrors)
  }

  def enterSyms(implicit ctx: Context): Unit = monitor("indexing") {
    val unit = ctx.compilationUnit
    ctx.typer.index(unit.untpdTree)
    typr.println("entered: " + unit.source)
  }

  def typeCheck(implicit ctx: Context): Unit = monitor("typechecking") {
    try
      val unit = ctx.compilationUnit
      if !unit.suspended then
        unit.tpdTree = ctx.typer.typedExpr(unit.untpdTree)
        typr.println("typed: " + unit.source)
        record("retained untyped trees", unit.untpdTree.treeSize)
        record("retained typed trees after typer", unit.tpdTree.treeSize)
    catch
      case ex: CompilationUnit.SuspendException =>
  }

  private def firstTopLevelDef(trees: List[tpd.Tree])(implicit ctx: Context): Symbol = trees match {
    case PackageDef(_, defs) :: _    => firstTopLevelDef(defs)
    case Import(_, _) :: defs        => firstTopLevelDef(defs)
    case (tree @ TypeDef(_, _)) :: _ => tree.symbol
    case _ => NoSymbol
  }

  protected def discardAfterTyper(unit: CompilationUnit)(implicit ctx: Context): Boolean =
    unit.isJava || unit.suspended

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
    val unitContexts = for (unit <- units) yield {
      ctx.inform(s"compiling ${unit.source}")
      ctx.fresh.setCompilationUnit(unit)
    }
    unitContexts.foreach(parse(_))
    record("parsedTrees", ast.Trees.ntrees)
    remaining = unitContexts
    while (remaining.nonEmpty) {
      enterSyms(remaining.head)
      remaining = remaining.tail
    }

    if (firstXmlPos.exists && !defn.ScalaXmlPackageClass.exists)
      ctx.error("""To support XML literals, your project must depend on scala-xml.
                  |See https://github.com/scala/scala-xml for more information.""".stripMargin,
        firstXmlPos)

    unitContexts.foreach(typeCheck(_))
    record("total trees after typer", ast.Trees.ntrees)
    val newUnits = unitContexts.map(_.compilationUnit).filterNot(discardAfterTyper)
    val suspendedUnits = ctx.run.suspendedUnits
    if newUnits.isEmpty && suspendedUnits.nonEmpty && !ctx.reporter.errorsReported then
      val where =
        if suspendedUnits.size == 1 then i"in ${suspendedUnits.head}."
        else i"""among
                |
                |  ${suspendedUnits.toList}%, %
                |"""
      ctx.error(em"""Cyclic macro dependencies $where
                    |Compilation stopped since no further progress can be made.
                    |
                    |To fix this, place macros in one set of files and their callers in another.
                    |
                    |Compiling with  -XprintSuspension   gives more information.""")
    newUnits
  }

  def run(implicit ctx: Context): Unit = unsupported("run")
}

object FrontEnd {
  val name: String = "frontend"
}
