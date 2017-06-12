package dotty.tools
package repl

import java.io.{ File => JFile }

import dotc.ast.untpd
import dotc.ast.tpd
import dotc.{ CompilationUnit, Compiler }
import dotc.core.{ Phases, Decorators, Flags }
import Decorators._, Flags._
import dotc.util.SourceFile
import dotc.typer.FrontEnd
import backend.jvm.GenBCode
import dotc.core.Contexts.Context
import dotc.util.Positions._
import dotc.reporting.diagnostic.MessageContainer
import dotc.reporting._
import io._

class ReplTyper(ictx: Context) extends Compiler {

  type NextRes = Int

  private class REPLFrontEnd extends FrontEnd {
    override def phaseName = "replFrontEnd"

    override def runOn(units: List[CompilationUnit])(implicit ctx: Context) = {
      val unitContexts = for (unit <- units) yield ctx.fresh.setCompilationUnit(unit)
      var remaining = unitContexts
      while (remaining.nonEmpty) {
        enterSyms(remaining.head)
        remaining = remaining.tail
      }
      unitContexts.foreach(enterAnnotations(_))
      unitContexts.foreach(typeCheck(_))
      unitContexts.map(_.compilationUnit).filterNot(discardAfterTyper)
    }
  }

  override def phases = List(new REPLFrontEnd :: Nil)

  def freeToAssigned(trees: Seq[untpd.Tree], currentRes: Int)
                    (implicit ctx: Context): (NextRes, Seq[untpd.Tree]) = {
    import untpd._

    def freeExpression(t: Tree) =
      t.isTerm && !t.isInstanceOf[Assign]

    val (exps, other) = trees.partition(freeExpression)
    val resX = exps.zipWithIndex.map { (exp, i) =>
      ValDef(s"res${i + currentRes}".toTermName, TypeTree(), exp)
        .withPos(exp.pos)
    }

    (currentRes + resX.length, resX ++ other)
  }

  def wrapped(trees: Seq[untpd.Tree])(implicit ctx: Context): untpd.TypeDef = {
    import untpd._

    val tmpl = Template(emptyConstructor, Nil, EmptyValDef, trees)
    TypeDef("TypeCheckingContext".toTypeName, tmpl)
      .withPos(Position(trees.head.pos.start, trees.last.pos.end))
  }

  def extractStats(tree: tpd.Tree)(implicit ctx: Context): Seq[tpd.Tree] =
    tree match {
      case tpd.TypeDef(_, tpl: tpd.Template) =>
        tpl.body.collect { case tree: tpd.Tree => tree }
      case _ => Nil
    }

  sealed trait Result
  case class TypedTrees(trees: Seq[tpd.Tree]) extends Result
  case class TypeErrors(msgs: Seq[MessageContainer]) extends Result

  def typeCheck(parsed: Parsed, currentRes: Int)(implicit ctx: Context): (NextRes, Result) = {
    val reporter = new StoreReporter(null) with UniqueMessagePositions with HideNonSensicalMessages

    val unit = new CompilationUnit(new SourceFile(s"repl-run-$currentRes", parsed.sourceCode))
    val (newRes, trees) = freeToAssigned(parsed.trees, currentRes)
    unit.untpdTree = wrapped(trees)

    val run = newRun(ctx.fresh.setReporter(reporter))
    run.compileUnits(unit :: Nil)

    val errs = reporter.removeBufferedMessages
    val res =
      if (errs.isEmpty) TypedTrees(extractStats(unit.tpdTree))
      else TypeErrors(errs)

    (newRes, res)
  }
}
