package test

import dotty.tools.dotc.core._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Flags._
import Types._, Symbols._, Decorators._
import dotty.tools.dotc.printing.Texts._
import dotty.tools.dotc.reporting.ConsoleReporter
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.Compiler

import dotty.tools.dotc
import dotty.tools.dotc.core.Phases.Phase

class DottyTest extends ContextEscapeDetection{

  dotty.tools.dotc.parsing.Scanners // initialize keywords

  implicit var ctx: Contexts.Context = {
    val base = new ContextBase {}
    import base.settings._
    val ctx = base.initialCtx.fresh
    base.initialize()(ctx)
    ctx.setSetting(ctx.settings.encoding, "UTF8")
    ctx
  }

  override def getCtx: Context = ctx
  override def clearCtx() = {
    ctx = null
  }
  private def compilerWithChecker(phase: String)(assertion:(tpd.Tree, Context) => Unit) = new Compiler {
    override def phases = {
      val allPhases = super.phases
      val targetPhase = allPhases.flatten.find(p => p.phaseName == phase).get
      val groupsBefore = allPhases.takeWhile(x => !x.contains(targetPhase))
      val lastGroup = allPhases.find(x => x.contains(targetPhase)).get.takeWhile(x => !(x eq targetPhase))
      val checker = new Phase {
        def phaseName = "assertionChecker"
        override def run(implicit ctx: Context): Unit = assertion(ctx.compilationUnit.tpdTree, ctx)
      }
      val lastGroupAppended = List(lastGroup ::: targetPhase :: Nil)

      groupsBefore ::: lastGroupAppended ::: List(List(checker))
    }
  }

  def checkCompile(checkAfterPhase: String, source: String)(assertion: (tpd.Tree, Context) => Unit): Unit = {
    val c = compilerWithChecker(checkAfterPhase)(assertion)
    c.rootContext(ctx)
    val run = c.newRun
    run.compile(source)
  }

  def checkCompile(checkAfterPhase: String, sources:List[String])(assertion:(tpd.Tree, Context) => Unit): Unit = {
    val c = compilerWithChecker(checkAfterPhase)(assertion)
    c.rootContext(ctx)
    val run = c.newRun
    run.compile(sources)
  }

  def methType(names: String*)(paramTypes: Type*)(resultType: Type = defn.UnitType) =
    MethodType(names.toList map (_.toTermName), paramTypes.toList, resultType)
}
