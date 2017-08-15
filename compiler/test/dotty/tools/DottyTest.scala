package dotty
package tools

import dotc.core._
import dotc.core.Contexts._
import dotc.core.Symbols._
import dotc.core.Flags._
import Types._, Symbols._, Decorators._
import dotc.printing.Texts._
import dotc.reporting.ConsoleReporter
import dotc.core.Decorators._
import dotc.ast.tpd
import dotc.Compiler

import dotc.core.Phases.Phase

trait DottyTest extends ContextEscapeDetection {

  dotc.parsing.Scanners // initialize keywords

  implicit var ctx: Context = initialCtx

  protected def initialCtx: FreshContext = {
    val base = new ContextBase {}
    import base.settings._
    val ctx = base.initialCtx.fresh
    initializeCtx(ctx)
    // when classpath is changed in ctx, we need to re-initialize to get the
    // correct classpath from PathResolver
    base.initialize()(ctx)
    ctx
  }

  override def getCtx: Context = ctx
  override def clearCtx() = {
    ctx = null
  }

  protected def initializeCtx(fc: FreshContext): Unit = {
    fc.setSetting(fc.settings.encoding, "UTF8")
    fc.setSetting(fc.settings.classpath, Jars.dottyLib)
  }

  private def compilerWithChecker(phase: String)(assertion: (tpd.Tree, Context) => Unit) = new Compiler {
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

  def checkCompile(checkAfterPhase: String, source: String)(assertion: (tpd.Tree, Context) => Unit): Context = {
    val c = compilerWithChecker(checkAfterPhase)(assertion)
    val run = c.newRun
    run.compile(source)
    run.runContext
  }

  def checkCompile(checkAfterPhase: String, sources: List[String])(assertion: (tpd.Tree, Context) => Unit): Context = {
    val c = compilerWithChecker(checkAfterPhase)(assertion)
    val run = c.newRun
    run.compile(sources)
    run.runContext
  }

  def methType(names: String*)(paramTypes: Type*)(resultType: Type = defn.UnitType) =
    MethodType(names.toList map (_.toTermName), paramTypes.toList, resultType)
}
