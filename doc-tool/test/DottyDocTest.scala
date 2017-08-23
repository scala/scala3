package dotty.tools
package dottydoc

import dotc.core.Contexts.{ Context, ContextBase, FreshContext }
import dotc.core.Comments.{ ContextDoc, ContextDocstrings }
import dotc.util.SourceFile
import dotc.core.Phases.Phase
import dotc.typer.FrontEnd
import dottydoc.core.{ DocASTPhase, ContextDottydoc }
import model.Package
import dotty.tools.dottydoc.util.syntax._
import dotc.reporting.{ StoreReporter, MessageRendering }
import dotc.interfaces.Diagnostic.ERROR
import org.junit.Assert.fail

import java.io.{ BufferedWriter, OutputStreamWriter }

trait DottyDocTest extends MessageRendering {
  dotty.tools.dotc.parsing.Scanners // initialize keywords

  implicit val ctx: FreshContext = {
    val base = new ContextBase
    import base.settings._
    val ctx = base.initialCtx.fresh
    ctx.setSetting(ctx.settings.language, List("Scala2"))
    ctx.setSetting(ctx.settings.YkeepComments, true)
    ctx.setSetting(ctx.settings.YnoInline, true)
    ctx.setSetting(ctx.settings.wikiSyntax, true)
    ctx.setProperty(ContextDoc, new ContextDottydoc)
    ctx.setSetting(
      ctx.settings.classpath,
      dotty.Jars.dottyLib
    )
    ctx.setReporter(new StoreReporter(ctx.reporter))
    base.initialize()(ctx)
    ctx
  }

  private def compilerWithChecker(assertion: Map[String, Package] => Unit) = new DocCompiler {
    private[this] val assertionPhase: List[List[Phase]] =
      List(new Phase {
        def phaseName = "assertionPhase"
        override def run(implicit ctx: Context): Unit =
          assertion(ctx.docbase.packages)
          if (ctx.reporter.hasErrors) {
            System.err.println("reporter had errors:")
            ctx.reporter.removeBufferedMessages.foreach { msg =>
              System.err.println {
                messageAndPos(msg.contained(), msg.pos, diagnosticLevel(msg))
              }
            }
          }
      }) :: Nil

    override def phases =
      super.phases ++ assertionPhase
  }

  private def callingMethod: String =
    Thread.currentThread.getStackTrace.find {
      _.getMethodName match {
        case "checkSource" | "callingMethod" | "getStackTrace" | "currentThread" =>
          false
        case _ =>
          true
      }
    }
    .map(_.getMethodName)
    .getOrElse {
      throw new IllegalStateException("couldn't get calling method via reflection")
    }

  private def sourceFileFromString(name: String, contents: String): SourceFile = {
    val virtualFile = new dotty.tools.io.VirtualFile(name)
    val writer = new BufferedWriter(new OutputStreamWriter(virtualFile.output, "UTF-8"))
    writer.write(contents)
    writer.close()
    new SourceFile(virtualFile, scala.io.Codec.UTF8)
  }

  def checkSource(source: String)(assertion: Map[String, Package] => Unit): Unit = {
    val c = compilerWithChecker(assertion)
    val run = c.newRun
    run.compileSources(sourceFileFromString(callingMethod, source) :: Nil)
  }

  def checkFiles(sources: List[String])(assertion: Map[String, Package] => Unit): Unit = {
    val c = compilerWithChecker(assertion)
    val run = c.newRun
    run.compile(sources)
  }

  def checkSources(sourceFiles: List[SourceFile])(assertion: Map[String, Package] => Unit): Unit = {
    val c = compilerWithChecker(assertion)
    val run = c.newRun
    run.compileSources(sourceFiles)
  }
}
