package dotty.tools
package dottydoc

import vulpix.TestConfiguration

import dotc.Compiler
import dotc.core.Contexts.{ Context, ContextBase, FreshContext }
import dotc.core.Comments.{ ContextDoc, ContextDocstrings }
import dotc.util.SourceFile
import dotc.core.Phases.Phase
import dotty.tools.io.AbstractFile
import dotc.typer.FrontEnd
import dottydoc.core.{ DocASTPhase, ContextDottydoc }
import model.Package
import dotty.tools.dottydoc.util.syntax._
import dotty.tools.io.AbstractFile
import dotc.reporting.{ StoreReporter, MessageRendering }
import dotc.interfaces.Diagnostic.ERROR
import io.{Directory, PlainFile, Path, PlainDirectory}
import org.junit.Assert.fail

import java.io.{ BufferedWriter, OutputStreamWriter }
import java.nio.charset.StandardCharsets

trait DottyDocTest extends MessageRendering {
  dotty.tools.dotc.parsing.Scanners // initialize keywords

  private def freshCtx(extraClasspath: List[String]): FreshContext = {
    val base = new ContextBase
    import base.settings._
    val ctx = base.initialCtx.fresh
    ctx.setSetting(ctx.settings.language, List("Scala2"))
    ctx.setSetting(ctx.settings.YcookComments, true)
    ctx.setSetting(ctx.settings.Ycheck, "all" :: Nil)
    ctx.setSetting(ctx.settings.wikiSyntax, true)
    ctx.setProperty(ContextDoc, new ContextDottydoc)
    ctx.setSetting(
      ctx.settings.classpath,
      (TestConfiguration.basicClasspath :: extraClasspath).mkString(java.io.File.pathSeparator)
    )
    ctx.setReporter(new StoreReporter(ctx.reporter))
    base.initialize()(using ctx)
    ctx
  }
  implicit val ctx: FreshContext = freshCtx(Nil)

  private def compilerWithChecker(assertion: (Context, Map[String, Package]) => Unit) = new DocCompiler {
    override def phases = {
      val assertionPhase = new Phase {
        def phaseName = "assertionPhase"
        override def run(implicit ctx: Context): Unit = {
          assertion(ctx, ctx.docbase.packages)
          if (ctx.reporter.hasErrors) {
            System.err.println("reporter had errors:")
            ctx.reporter.removeBufferedMessages.foreach { msg =>
              System.err.println {
                messageAndPos(msg.msg, msg.pos, diagnosticLevel(msg))
              }
            }
          }
        }
      }
      super.phases :+ List(assertionPhase)
    }
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
    val writer = new BufferedWriter(new OutputStreamWriter(virtualFile.output, StandardCharsets.UTF_8))
    writer.write(contents)
    writer.close()
    new SourceFile(virtualFile, scala.io.Codec.UTF8)
  }

  def checkSource(source: String)(assertion: (Context, Map[String, Package]) => Unit): Unit = {
    val c = compilerWithChecker(assertion)
    val run = c.newRun
    run.compileSources(sourceFileFromString(callingMethod, source) :: Nil)
  }

  def checkFiles(sources: List[String])(assertion: (Context, Map[String, Package]) => Unit): Unit = {
    val c = compilerWithChecker(assertion)
    val run = c.newRun
    val files = sources.map(path => new PlainFile(Path(path)))
    run.compile(files)
  }

  def checkFromSource(sourceFiles: List[SourceFile])(assertion: (Context, Map[String, Package]) => Unit): Unit = {
    val c = compilerWithChecker(assertion)
    val run = c.newRun
    run.compileSources(sourceFiles)
  }

  def checkFromTasty(tastyFiles: List[String], sources: List[SourceFile])(assertion: (Context, Map[String, Package]) => Unit): Unit = {
    Directory.inTempDirectory { tmp =>
      val ctx = "shadow ctx"
      val out = tmp./(Directory("out"))
      out.createDirectory()

      val dotcCtx = {
        val ctx = freshCtx(out.toString :: Nil)
        ctx.setSetting(ctx.settings.outputDir, AbstractFile.getDirectory(out))
      }
      val dotc = new Compiler
      val run = dotc.newRun(using dotcCtx)
      run.compileSources(sources)
      assert(!dotcCtx.reporter.hasErrors)

      val fromTastyCtx = {
        val ctx = freshCtx(out.toString :: Nil)
        ctx.setSetting(ctx.settings.fromTasty, true)
      }
      val fromTastyCompiler = compilerWithChecker(assertion)
      val fromTastyRun = fromTastyCompiler.newRun(using fromTastyCtx)
      val outDir = new PlainDirectory(out)
      val files = tastyFiles.map(outDir.fileNamed)
      fromTastyRun.compile(files)
      fromTastyCtx.reporter.allErrors.foreach(println)
      assert(!fromTastyCtx.reporter.hasErrors)
    }
  }

  def check(tastyFiles: List[String], sources: List[SourceFile])(assertion: (Context, Map[String, Package]) => Unit): Unit

}

trait CheckFromSource extends DottyDocTest {
  override def check(tastyFiles: List[String], sources: List[SourceFile])(assertion: (Context, Map[String, Package]) => Unit): Unit = {
    checkFromSource(sources)(assertion)
  }
}

trait CheckFromTasty extends DottyDocTest {
  override def check(tastyFiles: List[String], sources: List[SourceFile])(assertion: (Context, Map[String, Package]) => Unit): Unit = {
    checkFromTasty(tastyFiles, sources)(assertion)
  }
}
