package dotty.tools
package dottydoc

import dotty.tools.dottydoc.util.syntax._
import core.ContextDottydoc
import dotc.core.Contexts._
import dotc.reporting.Reporter
import dotc.{ Compiler, Driver }
import dotc.config._
import dotc.core.Comments.ContextDoc
import staticsite.Site

/** `DocDriver` implements the main entry point to the Dotty documentation
 *  tool. It's methods are used by the external scala and java APIs.
 */
class DocDriver extends Driver {
  import java.util.{ Map => JMap }
  import model.JavaConverters._

  override def setup(args: Array[String], rootCtx: Context): (List[String], Context) = {
    val ctx     = rootCtx.fresh
    val summary = CompilerCommand.distill(args)(ctx)

    ctx.setSettings(summary.sstate)
    ctx.setSetting(ctx.settings.YcookComments, true)
    ctx.setSetting(ctx.settings.YnoInline, true)
    ctx.setProperty(ContextDoc, new ContextDottydoc)

    val fileNames = CompilerCommand.checkUsage(summary, sourcesRequired)(ctx)
    fromTastySetup(fileNames, ctx)
  }

  override def newCompiler(implicit ctx: Context): Compiler = new DocCompiler

  override def process(args: Array[String], rootCtx: Context): Reporter = {
    val (filesToDocument, ictx) = setup(args, initCtx.fresh)

    implicit val ctx: Context = ictx
    val reporter = doCompile(newCompiler, filesToDocument)

    val siteRoot = new java.io.File(ctx.settings.siteRoot.value)
    val projectName = ctx.settings.projectName.value
    val projectVersion = ctx.settings.projectVersion.value
    val projectUrl = Option(ctx.settings.projectUrl.value).filter(_.nonEmpty)
    val projectLogo = Option(ctx.settings.projectLogo.value).filter(_.nonEmpty)

    if (projectName.isEmpty)
      ctx.error(s"Site project name not set. Use `-project <title>` to set the project name")
    else if (!siteRoot.exists || !siteRoot.isDirectory)
      ctx.error(s"Site root does not exist: $siteRoot")
    else {
      Site(siteRoot, projectName, projectVersion, projectUrl, projectLogo, ctx.docbase.packages)
        .generateApiDocs()
        .copyStaticFiles()
        .generateHtmlFiles()
        .generateBlog()

      ctx.docbase.printSummary()
    }

    reporter
  }
}
