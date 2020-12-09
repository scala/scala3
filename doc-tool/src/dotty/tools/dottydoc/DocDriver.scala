package dotty.tools
package dottydoc

import java.io.File

import dotty.tools.dottydoc.util.syntax._
import core.ContextDottydoc
import dotc.core.Contexts._
import dotc.reporting.Reporter
import dotc.{ Compiler, Driver }
import dotc.config._
import dotc.core.Comments.ContextDoc
import dotc.report
import staticsite.Site

/** `DocDriver` implements the main entry point to the Dotty documentation
 *  tool. It's methods are used by the external scala and java APIs.
 */
class DocDriver extends Driver {
  import java.util.{ Map => JMap }
  import model.JavaConverters._

  override def setup(args: Array[String], rootCtx: Context): (List[String], Context) = {
    val ctx     = rootCtx.fresh
    val summary = CompilerCommand.distill(args)(using ctx)

    ctx.setSettings(summary.sstate)
    ctx.setSetting(ctx.settings.YcookComments, true)
    ctx.setProperty(ContextDoc, new ContextDottydoc)

    val fileNames = CompilerCommand.checkUsage(summary, sourcesRequired)(using ctx)
    fromTastySetup(fileNames, ctx)
  }

  override def newCompiler(using Context): Compiler = new DocCompiler

  override def process(args: Array[String], rootCtx: Context): Reporter = {
    val (filesToDocument, ictx) = setup(args, initCtx.fresh)

    implicit val ctx: Context = ictx
    val reporter = doCompile(newCompiler, filesToDocument)

    val siteRoot = File(ctx.settings.siteRoot.value)
    val projectName = ctx.settings.projectName.value
    val projectVersion = ctx.settings.projectVersion.value
    val projectUrl = Option(ctx.settings.projectUrl.value).filter(_.nonEmpty)
    val projectLogo = Option(ctx.settings.projectLogo.value).filter(_.nonEmpty)
    val docSnapshot = ctx.settings.docSnapshot.value

    val baseUrl = ""
    val outDir = File(siteRoot, "_site")
    val snapshotFolderName = if projectVersion.endsWith("NIGHTLY") then "nightly" else projectVersion
    val snapshotOutDir = File(outDir, snapshotFolderName)
    val snapshotBaseUrl = s"$baseUrl/$snapshotFolderName"

    if (projectName.isEmpty)
      report.error(s"Site project name not set. Use `-project <title>` to set the project name")
    else if (!siteRoot.exists)
      report.echo(s"Site root (`-siteroot`) does not exist: $siteRoot, no documentation will be generated.")
    else if (!siteRoot.isDirectory)
      report.error(s"Site root (`-siteroot`) is not a directory: $siteRoot")
    else {
      def generateSite(outDir: File, baseUrl: String) =
        Site(siteRoot, outDir, projectName, projectVersion, projectUrl, projectLogo, ctx.docbase.packages, baseUrl)
          .generateApiDocs()
          .copyStaticFiles()
          .generateHtmlFiles()
          .generateBlog()

      generateSite(outDir, baseUrl)
      if docSnapshot then generateSite(snapshotOutDir, snapshotBaseUrl)
      ctx.docbase.printSummary()
    }

    reporter
  }
}
