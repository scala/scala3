package dotty.tools.dotc.parsing

import dotty.tools.dotc.config.Config
import dotty.tools.dotc.core.Contexts.{ Context, ctx }
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.typer.ImportInfo.withRootImports
import dotty.tools.dotc.{ CompilationUnit, ast, report }
import dotty.tools.dotc.util.{ NoSourcePosition, SourcePosition }
import dotty.tools.dotc.util.Stats.record
import dotty.tools.unsupported

class Parser extends Phase {

  override def phaseName: String = Parser.name
  override def description: String = Parser.description

  // We run TreeChecker only after type checking
  override def isCheckable: Boolean = false

  /** The position of the first XML literal encountered while parsing,
   *  NoSourcePosition if there were no XML literals.
   */
  private[dotc] var firstXmlPos: SourcePosition = NoSourcePosition

  def parse(using Context): Boolean = monitor("parser") {
    val unit = ctx.compilationUnit
    unit.untpdTree =
      if (unit.isJava) new JavaParsers.JavaParser(unit.source).parse()
      else {
        val p = new Parsers.Parser(unit.source)
        //  p.in.debugTokenStream = true
        val tree = p.parse()
        ctx.compilationUnit.comments = p.in.comments
        if (p.firstXmlPos.exists && !firstXmlPos.exists)
          firstXmlPos = p.firstXmlPos
        tree
      }
    if (Config.checkPositions)
      unit.untpdTree.checkPos(nonOverlapping = !unit.isJava && !ctx.reporter.hasErrors)
  }


  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] = {
    val unitContexts =
      for unit <- units yield
        report.inform(s"parsing ${unit.source}")
        ctx.fresh.setCompilationUnit(unit).withRootImports

    val unitContexts0 =
      for
        unitContext <- unitContexts
        if parse(using unitContext)
      yield unitContext

    record("parsedTrees", ast.Trees.ntrees)

    unitContexts0.map(_.compilationUnit)
  }

  protected def run(using Context): Unit = unsupported("run")
}

object Parser{
  val name: String = "parser"
  val description: String = "scan and parse sources"
}
