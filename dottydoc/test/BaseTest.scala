package dotty.tools
package dottydoc

import dotc.core.Contexts
import Contexts.{ Context, ContextBase, FreshContext, DocContext, DocBase }
import dotc.util.SourceFile
import dotc.core.Phases.Phase
import dotc.typer.FrontEnd
import dottydoc.core.DocASTPhase
import model.Package
import dotty.tools.dottydoc.util.syntax._

trait DottyTest {
  dotty.tools.dotc.parsing.Scanners // initialize keywords

  implicit val ctx: FreshContext = {
    val base = new ContextBase
    import base.settings._
    val ctx = base.initialCtx.fresh
    ctx.setSetting(ctx.settings.language, List("Scala2"))
    ctx.setSetting(ctx.settings.YnoInline, true)
    ctx.setSetting(ctx.settings.YkeepComments, true)
    ctx.setProperty(DocContext, new DocBase)
    base.initialize()(ctx)
    ctx
  }

  private def compilerWithChecker(assertion: Map[String, Package] => Unit) = new DocCompiler {
    private[this] val assertionPhase: List[List[Phase]] =
      List(new Phase {
        def phaseName = "assertionPhase"
        override def run(implicit ctx: Context): Unit =
          assertion(ctx.docbase.packages)
      }) :: Nil

    override def phases =
      super.phases ++ assertionPhase
  }

  def checkSource(source: String)(assertion: Map[String, Package] => Unit): Unit = {
    val c = compilerWithChecker(assertion)
    c.rootContext(ctx)
    val run = c.newRun
    run.compile(source)
  }

  def checkFiles(sources: List[String])(assertion: Map[String, Package] => Unit): Unit = {
    val c = compilerWithChecker(assertion)
    c.rootContext(ctx)
    val run = c.newRun
    run.compile(sources)
  }

  def checkSources(sourceFiles: List[SourceFile])(assertion: Map[String, Package] => Unit): Unit = {
    val c = compilerWithChecker(assertion)
    c.rootContext(ctx)
    val run = c.newRun
    run.compileSources(sourceFiles)
  }
}
