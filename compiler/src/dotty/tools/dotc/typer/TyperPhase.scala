package dotty.tools
package dotc
package typer

import core._
import Phases._
import Contexts._
import Symbols._
import ImportInfo.withRootImports
import parsing.{Parser => ParserPhase}
import config.Printers.typr
import util.Stats._

/**
 *
 * @param addRootImports Set to false in the REPL. Calling [[ImportInfo.withRootImports]] on the [[Context]]
 *                       for each [[CompilationUnit]] causes dotty.tools.repl.ScriptedTests to fail.
 */
class TyperPhase(addRootImports: Boolean = true) extends Phase {

  override def phaseName: String = TyperPhase.name

  override def description: String = TyperPhase.description

  override def isTyper: Boolean = true


  override def allowsImplicitSearch: Boolean = true

  // Run regardless of parsing errors
  override def isRunnable(implicit ctx: Context): Boolean = true

  def enterSyms(using Context): Unit = monitor("indexing") {
    val unit = ctx.compilationUnit
    ctx.typer.index(unit.untpdTree)
    typr.println("entered: " + unit.source)
  }

  def typeCheck(using Context): Unit = monitor("typechecking") {
    try
      val unit = ctx.compilationUnit
      if !unit.suspended then
        unit.tpdTree = ctx.typer.typedExpr(unit.untpdTree)
        typr.println("typed: " + unit.source)
        record("retained untyped trees", unit.untpdTree.treeSize)
        record("retained typed trees after typer", unit.tpdTree.treeSize)
        ctx.run.suppressions.reportSuspendedMessages(unit.source)
    catch
      case ex: CompilationUnit.SuspendException =>
  }

  def javaCheck(using Context): Unit = monitor("checking java") {
    val unit = ctx.compilationUnit
    if unit.isJava then
      JavaChecks.check(unit.tpdTree)
  }

  protected def discardAfterTyper(unit: CompilationUnit)(using Context): Boolean =
    unit.isJava || unit.suspended

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    val unitContexts =
      for unit <- units yield
        val newCtx0 = ctx.fresh.setPhase(this.start).setCompilationUnit(unit)
        val newCtx = PrepareInlineable.initContext(newCtx0)
        report.inform(s"typing ${unit.source}")
        if (addRootImports)
          newCtx.withRootImports
        else
          newCtx

    unitContexts.foreach(enterSyms(using _))

    ctx.base.parserPhase match {
      case p: ParserPhase =>
        if p.firstXmlPos.exists && !defn.ScalaXmlPackageClass.exists then
          report.error(
            """To support XML literals, your project must depend on scala-xml.
              |See https://github.com/scala/scala-xml for more information.""".stripMargin,
            p.firstXmlPos)
      case _ =>
    }

    unitContexts.foreach(typeCheck(using _))
    record("total trees after typer", ast.Trees.ntrees)
    unitContexts.foreach(javaCheck(using _)) // after typechecking to avoid cycles

    val newUnits = unitContexts.map(_.compilationUnit).filterNot(discardAfterTyper)
    ctx.run.checkSuspendedUnits(newUnits)
    newUnits

  def run(using Context): Unit = unsupported("run")
}

object TyperPhase {
  val name: String = "typer"
  val description: String = "type the trees"
}

@deprecated(message = "FrontEnd has been split into TyperPhase and Parser. Refer to one or the other.")
object FrontEnd {
  // For backwards compatibility: some plugins refer to FrontEnd so that they can schedule themselves after it.
  val name: String = TyperPhase.name
}
