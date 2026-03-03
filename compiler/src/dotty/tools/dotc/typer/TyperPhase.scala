package dotty.tools
package dotc
package typer

import core.*
import Run.SubPhase
import Phases.*
import Contexts.*
import Symbols.*
import ImportInfo.withRootImports
import parsing.{Parser => ParserPhase}
import config.Printers.typr
import inlines.PrepareInlineable
import util.Stats.*
import dotty.tools.dotc.config.Feature
import dotty.tools.dotc.config.SourceVersion

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

  def enterSyms(using Context)(using subphase: SubPhase): Boolean = monitor(subphase.name) {
    val unit = ctx.compilationUnit
    ctx.typer.index(unit.untpdTree)
    typr.println("entered: " + unit.source)
  }

  def typeCheck(using Context)(using subphase: SubPhase): Boolean = monitor(subphase.name) {
    val unit = ctx.compilationUnit
    try
      if !unit.suspended then ctx.profiler.onUnit(ctx.phase, unit):
        unit.tpdTree = ctx.typer.typedExpr(unit.untpdTree)
        typr.println("typed: " + unit.source)
        record("retained untyped trees", unit.untpdTree.treeSize)
        record("retained typed trees after typer", unit.tpdTree.treeSize)
        ctx.run.nn.suppressions.reportSuspendedMessages(unit.source)
    catch case _: CompilationUnit.SuspendException => ()
  }

  def javaCheck(using Context)(using subphase: SubPhase): Boolean = monitor(subphase.name) {
    val unit = ctx.compilationUnit
    if unit.isJava then
      JavaChecks.check(unit.tpdTree)
  }

  protected def discardAfterTyper(unit: CompilationUnit)(using Context): Boolean =
    (unit.isJava && !ctx.settings.XjavaTasty.value) || unit.suspended

  override val subPhases: List[SubPhase] = List(
    SubPhase("indexing"), SubPhase("typechecking"), SubPhase("checkingJava"))

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    val List(Indexing @ _, Typechecking @ _, CheckingJava @ _) = subPhases: @unchecked
    val unitContexts =
      for unit <- units yield
        val newCtx0 = ctx.fresh.setPhase(this.start).setCompilationUnit(unit)
        val newCtx = PrepareInlineable.initContext(newCtx0)
        report.inform(s"typing ${unit.source}")
        if (addRootImports)
          newCtx.withRootImports
        else
          newCtx

    val unitContexts0 = runSubPhase(Indexing) {
      for
        unitContext <- unitContexts
        if enterSyms(using unitContext)
      yield unitContext
    }

    ctx.base.parserPhase match {
      case p: ParserPhase =>
        if p.firstXmlPos.exists && !defn.ScalaXmlPackageClass.exists && Feature.sourceVersion == SourceVersion.future then
          report.error(
            """To support XML literals, your project must depend on scala-xml.
              |See https://github.com/scala/scala-xml for more information.""".stripMargin,
            p.firstXmlPos)
      case _ =>
    }

    val unitContexts1 = runSubPhase(Typechecking) {
      for
        unitContext <- unitContexts0
        if typeCheck(using unitContext)
      yield unitContext
    }

    record("total trees after typer", ast.Trees.ntrees)

    val unitContexts2 = runSubPhase(CheckingJava) {
      for
        unitContext <- unitContexts1
        if javaCheck(using unitContext) // after typechecking to avoid cycles
      yield unitContext
    }
    val newUnits = unitContexts2.map(_.compilationUnit).filterNot(discardAfterTyper)
    ctx.run.nn.checkSuspendedUnits(newUnits)
    newUnits

  protected def run(using Context): Unit = unsupported("run")
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
