package dotty.dokka.site

import java.io.File
import java.nio.file.Files

import com.vladsch.flexmark.ext.anchorlink.AnchorLinkExtension
import com.vladsch.flexmark.ext.autolink.AutolinkExtension
import com.vladsch.flexmark.ext.emoji.EmojiExtension
import com.vladsch.flexmark.ext.gfm.strikethrough.StrikethroughExtension
import com.vladsch.flexmark.ext.gfm.tasklist.TaskListExtension
import com.vladsch.flexmark.ext.tables.TablesExtension
import com.vladsch.flexmark.ext.yaml.front.matter.{AbstractYamlFrontMatterVisitor, YamlFrontMatterExtension}
import com.vladsch.flexmark.parser.{Parser, ParserEmulationProfile}
import com.vladsch.flexmark.util.options.{DataHolder, MutableDataSet}
import com.vladsch.flexmark.html.HtmlRenderer
import liqp.Template

import scala.collection.JavaConverters._
import scala.io.Source

case class RenderingContext(
                             properties: Map[String, Object],
                             layouts: Map[String, TemplateFile] = Map(),
                             resolving: Set[String] = Set(),
                             markdownOptions: DataHolder = defaultMarkdownOptions,
                             resources: List[String] = Nil
                           ):

  def nest(code: String, file: File, resources: List[String]) =
    copy(
      resolving = resolving + file.getAbsolutePath,
      properties = properties + ("content" -> code),
      resources = this.resources ++ resources
    )

case class LayoutInfo(htmlTemple: TemplateFile, ctx: RenderingContext)

case class PreResolvedPage(
                            code: String,
                            nextLayoutInfo: Option[LayoutInfo],
                            hasMarkdown: Boolean,
                            resources: List[String] = Nil
                          ):
  def render(renderedMarkdown: String): ResolvedPage = nextLayoutInfo match
    case None => ResolvedPage(renderedMarkdown, hasMarkdown, resources)
    case Some(nextLayoutInfo) =>
      val newCtx =
        nextLayoutInfo.ctx.copy(properties = nextLayoutInfo.ctx.properties + ("content" -> renderedMarkdown))
      val res = nextLayoutInfo.htmlTemple.resolveToHtml(newCtx, hasMarkdown)
      ResolvedPage(res.code, hasMarkdown, res.resources)

case class ResolvedPage(
                         val code: String,
                         val hasMarkdown: Boolean,
                         val resources: List[String] = Nil
                       )

/**
 * case class for the template files.
 * Template file is a file `.md` or `.html` handling settings.
 *
 * @param file     The Actual file defining the template.
 * @param rawCode  The content, what is to be shown, everything but settings.
 * @param settings The config defined in the begging of the file, between the pair of `---` (e.g. layout: basic).
 */
case class TemplateFile(
                         val file: File,
                         val isHtml: Boolean,
                         val rawCode: String,
                         private val settings: Map[String, List[String]]
                       ):

  private def stringSetting(name: String): Option[String] =
    settings.get(name) map {
      case List(single) => single.stripPrefix("\"").stripSuffix("\"")
      case nonSingle =>
        throw new RuntimeException(s"Setting $name is a not a singlel-ement list but $nonSingle")
    }


  private def listSetting(name: String): List[String] = settings.getOrElse(name, Nil)

  def name(): String = stringSetting("name").getOrElse(file.getName.stripSuffix(if (isHtml) ".html" else ".md"))

  def title(): String = stringSetting("title").getOrElse(name())

  def layout(): Option[String] = stringSetting("layout")

  def hasFrame(): Boolean = !stringSetting("hasFrame").contains("false")


  def resolveMarkdown(ctx: RenderingContext): PreResolvedPage =
    resolveInner(
      ctx = ctx.copy(properties = ctx.properties + ("page" -> Map("title" -> title()))),
      stopAtHtml = true,
      !isHtml // This is top level template
    )

  def resolveToHtml(ctx: RenderingContext, hasMarkdown: Boolean): PreResolvedPage =
    resolveInner(ctx, stopAtHtml = false, hasMarkdown)

  private def resolveInner(ctx: RenderingContext, stopAtHtml: Boolean, hasMarkdown: Boolean): PreResolvedPage =
    if (ctx.resolving.contains(file.getAbsolutePath))
      throw new RuntimeException("Cycle in templates involving $file: ${ctx.resolving}")

    val layoutTemplate = layout().map(name =>
      ctx.layouts.getOrElse(name, throw new RuntimeException(s"No layouts named $name in ${ctx.layouts}")))

    if (!stopAtHtml && !isHtml)
      throw new java.lang.RuntimeException(
        "Markdown layout cannot be applied after .html. Rendering $file after: ${ctx.resolving}"
      )

    if stopAtHtml && isHtml then
      PreResolvedPage(ctx.properties.getOrElse("content", "").toString, Some(LayoutInfo(this, ctx)), hasMarkdown)
    else 
      val rendered = Template.parse(this.rawCode).render(ctx.properties.asJava) // Library requires mutable maps..
      val code = if (!isHtml) rendered else
        val parser: Parser = Parser.builder().build()
        HtmlRenderer.builder(ctx.markdownOptions).build().render(parser.parse(rendered))
      
      val resources = listSetting("extraCSS") ++ listSetting("extraJS")
      layoutTemplate match 
        case None => PreResolvedPage(code, None, hasMarkdown, resources ++ ctx.resources)
        case Some(layoutTemplate) =>
          layoutTemplate.resolveInner(ctx.nest(code, file, resources), stopAtHtml, hasMarkdown)
