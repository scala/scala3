package dotty.tools
package dotc
package typer

import core._
import Phases._
import Contexts._
import Symbols._
import config.Config
import config.Printers.{typr, default}
import util.Stats._
import scala.util.control.NonFatal
import ast.{tpd, Trees}
import Trees._

class FrontEnd extends Phase {
  import tpd._

  override def phaseName: String = FrontEnd.name
  override def isTyper: Boolean = true

  // Run regardless of parsing errors
  override def isRunnable(implicit ctx: Context): Boolean = true

  override def allowsImplicitSearch: Boolean = true

  /** The contexts for compilation units that are parsed but not yet entered */
  private[this] var remaining: List[Context] = Nil

  /** Does a source file ending with `<name>.scala` belong to a compilation unit
   *  that is parsed but not yet entered?
   */
  def stillToBeEntered(name: String): Boolean =
    remaining.exists(_.compilationUnit.toString.endsWith(name + ".scala"))

  private def enterSyms(implicit ctx: Context): Unit = monitor("indexing") {
    val unit = ctx.compilationUnit
    ctx.typer.index(unit.untpdTree)
    typr.println("entered: " + unit.source)
  }

  private def typeCheck(implicit ctx: Context): Unit = monitor("typechecking") {
    val unit = ctx.compilationUnit
    unit.tpdTree = ctx.typer.typedExpr(unit.untpdTree)
    typr.println("typed: " + unit.source)
    record("retained untyped trees", unit.untpdTree.treeSize)
    record("retained typed trees after typer", unit.tpdTree.treeSize)
  }

  private def firstTopLevelDef(trees: List[Tree])(implicit ctx: Context): Symbol = trees match {
    case PackageDef(_, defs) :: _    => firstTopLevelDef(defs)
    case Import(_, _, _) :: defs     => firstTopLevelDef(defs)
    case (tree @ TypeDef(_, _)) :: _ => tree.symbol
    case _ => NoSymbol
  }

  protected def discardAfterTyper(unit: CompilationUnit)(implicit ctx: Context): Boolean =
    unit.isJava || firstTopLevelDef(unit.tpdTree :: Nil).isPrimitiveValueClass

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
    val unitContexts = for (unit <- units) yield {
      ctx.inform(s"compiling ${unit.source}")
      ctx.fresh.setCompilationUnit(unit)
    }

    remaining = unitContexts
    while (remaining.nonEmpty) {
      enterSyms(remaining.head)
      remaining = remaining.tail
    }

    unitContexts.foreach(typeCheck(_))
    record("total trees after typer", Trees.ntrees)
    unitContexts.map(_.compilationUnit).filterNot(discardAfterTyper)
  }

  def run(implicit ctx: Context): Unit = unsupported("run")
}

object FrontEnd {
  val name: String = "frontend"
}
