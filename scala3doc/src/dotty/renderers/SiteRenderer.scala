package dotty.dokka
package renderers

import HTML._
import collection.JavaConverters._
import java.net.URI
import java.net.URL
import dotty.dokka.model.api._
import dotty.dokka.site._
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
    Page(Link(t.templateFile.title, dri), content, t.children.map(templateToPage(_, staticSiteCtx)))

  private val HashRegex = "([^#]+)(#.+)".r

  def siteContent(pageDri: DRI, content: ResolvedTemplate): AppliedTag =
    import content.ctx
    def tryAsDri(str: String) = // TODO Does not seem to work with links to API :(
      val (path, prefix) = str match
        case HashRegex(path, prefix) => (path, prefix)
        case _ => (str, "")

      val res = ctx.driForLink(content.template.templateFile, path).filter(driExisits)
      if res.isEmpty then report.warn(s"Unable to resolve link '$str'", content.template.file)
      res.headOption.fold(str)(pathToPage(_, pageDri) + prefix)

    def processLocalLink(str: String): String =
      if str.startsWith("#") || str.isEmpty then str
      else Try(URL(str)).map(_ => str).getOrElse(tryAsDri(str))

    val document = Jsoup.parse(content.resolved.code)
    document.select("a").forEach(element =>
      element.attr("href", processLocalLink(element.attr("href")))
    )
    document.select("img").forEach { element =>
      val link = element.attr("src")
      Try(new URL(link)).getOrElse {
        if(link.startsWith("/")) element.attr("src", resolveLink(pageDri, link.drop(1)))
      }
    }// forrach does not work here
    raw(document.outerHtml())
