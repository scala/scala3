package dotty.tools
package dotc
package typer

import core.*
import Phases.*
import Contexts.*
import Symbols.*
import Decorators.*
import ImportInfo.withRootImports
import parsing.JavaParsers.JavaParser
import parsing.Parsers.Parser
import parsing.Parser as ParserPhase
import config.Config
import config.Printers.{ default, typr }
import util.Stats.*
import util.{ NoSourcePosition, SourcePosition }
import scala.util.control.NonFatal

import ast.Trees.*
import dotty.tools.dotc.core.Denotations.SingleDenotation

/**
 *
 * @param addRootImports Set to false in the REPL. Calling [[ImportInfo.withRootImports]] on the [[Context]]
 *                       for each [[CompilationUnit]] causes dotty.tools.repl.ScriptedTests to fail.
 */
class NamerPhase(addRootImports: Boolean = true) extends Phase {

  override def phaseName: String = NamerPhase.name
  override def isTyper: Boolean = true

  // We run TreeChecker only after type checking
  override def isCheckable: Boolean = false

  override def allowsImplicitSearch: Boolean = true

  // Run regardless of parsing errors
  override def isRunnable(implicit ctx: Context): Boolean = true

  def enterSyms(using Context): Unit = monitor("indexing") {
    val unit = ctx.compilationUnit
    ctx.typer.index(unit.untpdTree)
    typr.println("entered: " + unit.source)
  }

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    val unitContexts =
      for unit <- units yield
        val newCtx0 = ctx.fresh.setPhase(this.start).setCompilationUnit(unit)
        val newCtx = PrepareInlineable.initContext(newCtx0)
        report.inform(s"naming ${unit.source}")
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


    unitContexts.map(_.compilationUnit)

  def run(using Context): Unit = unsupported("run")
}

object NamerPhase {
  val name: String = "namer"
}
