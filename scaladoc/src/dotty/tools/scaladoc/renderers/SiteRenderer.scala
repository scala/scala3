package dotty.tools.scaladoc
package renderers

import util.HTML._
import collection.JavaConverters._
import java.net.URI
import java.net.URL
import dotty.tools.scaladoc.site._
import scala.util.Try
import org.jsoup.Jsoup
import java.nio.file.Paths
import java.nio.file.Path
import java.nio.file.Files
import java.io.File

case class ResolvedTemplate(template: LoadedTemplate, ctx: StaticSiteContext):
  val resolved = template.resolveToHtml(ctx)
  def hasFrame = template.templateFile.hasFrame

trait SiteRenderer(using DocContext) extends Locations:

  def templateToPage(t: LoadedTemplate, staticSiteCtx: StaticSiteContext): Page =
    val dri = staticSiteCtx.driFor(t.file.toPath)
    val content = ResolvedTemplate(t, staticSiteCtx)
    Page(Link(t.templateFile.title.name, dri), content, t.children.map(templateToPage(_, staticSiteCtx)))

  private val HashRegex = "([^#]+)(#.+)".r

  def siteContent(pageDri: DRI, content: ResolvedTemplate): AppliedTag =
    import content.ctx
    def tryAsDri(str: String): Option[String] =
      val (path, prefix) = str match
        case HashRegex(path, prefix) => (path, prefix)
        case _ => (str, "")

      val res = ctx.driForLink(content.template.file, path).filter(driExists)
      res.headOption.map(pathToPage(pageDri, _) + prefix)

    def processLocalLink(str: String): String =
      val staticSiteRootPath = content.ctx.root.toPath.toAbsolutePath
      def asValidURL: Option[String] = Try(URL(str)).toOption.map(_ => str)
      def asAsset: Option[String] = Option.when(
        Files.exists(staticSiteRootPath.resolve("_assets").resolve(str.stripPrefix("/")))
      )(
        resolveLink(pageDri, str.stripPrefix("/"))
      )
      def asStaticSite: Option[String] = tryAsDri(str)

      /* Link resolving checks performs multiple strategies with following priority:
        1. We check if the link is a valid URL e.g. http://dotty.epfl.ch
        2. We check if the link leads to other static site
        3. We check if the link leads to existing asset e.g. images/logo.svg -> <static-site-root>/_assets/images/logo.svg
      */

      asValidURL
        .orElse(asStaticSite)
        .orElse(asAsset)
        .getOrElse {
          report.warn(s"Unable to resolve link '$str'", content.template.file)
          str
        }

    def processLocalLinkWithGuard(str: String): String =
      if str.startsWith("#") || str.isEmpty then
        str
      else
        processLocalLink(str)

    val document = Jsoup.parse(content.resolved.code)
    document.select("a").forEach(element =>
      element.attr("href", processLocalLinkWithGuard(element.attr("href")))
    )
    document.select("img").forEach { element =>
      element.attr("src", processLocalLink(element.attr("src")))
    } // foreach does not work here. Why?
    raw(document.outerHtml())
