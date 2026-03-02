package dotty.tools.scaladoc
package renderers

import util.HTML._
import scala.jdk.CollectionConverters._
import java.net.{URI, URL}
import dotty.tools.scaladoc.site._
import scala.util.Try
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import java.nio.file.Paths
import java.nio.file.Path
import java.nio.file.Files
import java.io.File
import scala.util.chaining._
import dotty.tools.scaladoc.util.Escape.escapeFilename

case class ResolvedTemplate(template: LoadedTemplate, ctx: StaticSiteContext):
  val resolved = template.resolveToHtml(ctx)
  def hasFrame = template.templateFile.hasFrame

trait SiteRenderer(using DocContext) extends Locations:

  def templateToPage(t: LoadedTemplate, staticSiteCtx: StaticSiteContext): Page =
    val dri = staticSiteCtx.driFor(t.file.toPath)
    val content = ResolvedTemplate(t, staticSiteCtx)
    Page(Link(t.templateFile.title.name, dri), content, t.children.map(templateToPage(_, staticSiteCtx)), t.hidden)

  private val HashRegex = "([^#]+)(#.+)".r

  def siteContent(pageDri: DRI, content: ResolvedTemplate): PageContent =
    import content.ctx

    def tryAsDriPlain(str: String): Option[String] =
      val (path, prefix) = str match
        case HashRegex(path: String, prefix: String) => (path, prefix)
        case _ => (str, "")
      val res = ctx.driForLink(content.template.file, path).filter(driExists)
      res.headOption.map(pathToPage(pageDri, _) + prefix)

    def tryAsDri(str: String): Option[String] =
      val newStr =
        str.dropWhile(c => c == '.' || c == '/').replaceAll("/", ".") match
          case str if str.endsWith("$.html") => str.stripSuffix("$.html")
          case str if str.endsWith(".html") => str.stripSuffix(".html")
          case _ => str

      val (path, prefix) = newStr match
        case HashRegex(path: String, prefix: String) => (path, prefix)
        case _ => (newStr, "")

      val res = ctx.driForLink(content.template.file, path).filter(driExists)
      res.headOption.map(pathToPage(pageDri, _) + prefix)

    def processLocalLink(str: String): String =
      val staticSiteRootPath = content.ctx.root.toPath.toAbsolutePath
      def asValidURL: Option[String] = Try(URI(str).toURL).toOption.map(_ => str)
      def asAsset: Option[String] = Option.when(
        Try(
          Files.exists(staticSiteRootPath.resolve("_assets").resolve(str.stripPrefix("/")))
        ).getOrElse(false)
      )(
        resolveLink(pageDri, str.stripPrefix("/"))
      )
      def asStaticSite: Option[String] =
        tryAsDriPlain(str)
          .orElse(tryAsDri(str))
          .orElse(tryAsDriPlain(escapeFilename(str)))

      /* Link resolving checks performs multiple strategies with following priority:
        1. We check if the link is a valid URL e.g. https://nightly.scala-lang.org
        2. We check if the link leads to other static site or API pages, example: [[exemple.scala.Foo]] || [Foo](../exemple/scala/Foo.html)
        3. We check if the link leads to existing asset e.g. images/logo.svg -> <static-site-root>/_assets/images/logo.svg
      */

      asValidURL
        .orElse(asStaticSite)
        .orElse(asAsset)
        .getOrElse {
          if (!summon[DocContext].args.noLinkAssetWarnings){
            val msg = s"Unable to resolve link '$str'"
            report.warn(msg, content.template.templateFile.file)
          }
          str
        }

    def processLocalLinkWithGuard(str: String): String =
      if str.startsWith("#") || str.isEmpty then
        str
      else
        processLocalLink(str)

    val document = Jsoup.parse(content.resolved.code)

    val toc = document.select("section[id]").asScala.toSeq
      .flatMap { elem =>
        val header = elem.selectFirst("h1, h2, h3, h4, h5, h6")
        Option(header).map { h =>
          TocEntry(h.tag().getName, h.text(), s"#${elem.id()}")
        }
      }

    document.select("header + p").forEach(firstParagraph =>
      firstParagraph.addClass("body-large")
      firstParagraph.addClass("first-p")
    )
    document.select("a").forEach(element =>
      element.attr("href", processLocalLinkWithGuard(element.attr("href")))
    )
    document.select("img").forEach { element =>
      element.attr("src", processLocalLink(element.attr("src")))
    } // foreach does not work here. Why?
    PageContent(div(raw(document.outerHtml())), toc)
