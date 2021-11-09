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
      Try(URL(str)).map(_ => str).toOption.orElse {
        tryAsDri(str)
      }.orElse {
        Option.when(
          Files.exists(Paths.get(content.ctx.root.toPath.toAbsolutePath.toString, str))
        )(
          resolveLink(pageDri, str.stripPrefix("/"))
        )
      }.getOrElse {
        report.warn(s"Unable to resolve link '$str'", content.template.file)
        str
      }

    def processLocalLinkWithGuard(str: String): String =
      if str.startsWith("#") || str.isEmpty then
        str
      else
        processLocalLink(str)

    summon[DocContext].args.projectFormat match
      case "html" =>
        val document = Jsoup.parse(content.resolved.code)
        document.select("a").forEach(element =>
          element.attr("href", processLocalLinkWithGuard(element.attr("href")))
        )
        document.select("img").forEach { element =>
          element.attr("src", processLocalLink(element.attr("src")))
        } // foreach does not work here. Why?
        raw(document.outerHtml())
      case "md" =>
        val links = """(?<!\\!)\[(.*)\\]\\((.*)\\)""".r
        val imgs = """!\\[(.*)\\]\\((.*)\\)""".r

        raw(links.replaceAllIn(
          imgs.replaceAllIn(
            content.resolved.code,
            m => s"[${m.group(1)}](${processLocalLink(m.group(2))})"
          ),
          m => s"[${m.group(1)}](${processLocalLinkWithGuard(m.group(2))})"
        ))
