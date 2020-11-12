package dotty.dokka
package site

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
import java.util.{ Map => JMap, HashMap => HMap, ArrayList, Collection }

import scala.collection.JavaConverters._
import scala.io.Source

case class RenderingContext(
  properties: Map[String, Element],
  layouts: Map[String, TemplateFile] = Map(),
  resolving: Set[String] = Set(),
  resources: List[String] = Nil
):

  def nest(code: String, file: File, resources: List[String]) =
    copy(
      resolving = resolving + file.getAbsolutePath,
      properties = properties + ("content" -> code),
      resources = this.resources ++ resources
    )

case class ResolvedPage(code: String, resources: List[String] = Nil)
type Element = String | List[String] | Object // Object represents Map[String, Element] since it is not possible to get circullar dependency
/**
 * case class for the template files.
 * Template file is a file `.md` or `.html` handling settings.
 *
 * @param file     The Actual file defining the template.
 * @param rawCode  The content, what is to be shown, everything but settings.
 * @param settings The config defined in the begging of the file, between the pair of `---` (e.g. layout: basic).
 */
case class TemplateFile(
  file: File,
  isHtml: Boolean,
  rawCode: String,
  settings: Map[String, Element],
  name: String,
  title: String,
  hasFrame: Boolean,
  resources: List[String],
  layout: Option[String],
):
  def isIndexPage() = file.isFile && (file.getName == "index.md" || file.getName == "index.html")

  def resolveToHtml(ctx: StaticSiteContext): ResolvedPage = resolveInner(RenderingContext(settings, ctx.layouts))

  private[site] def resolveInner(ctx: RenderingContext): ResolvedPage =
    if (ctx.resolving.contains(file.getAbsolutePath))
      throw new RuntimeException(s"Cycle in templates involving $file: ${ctx.resolving}")

    val layoutTemplate = layout.map(name =>
      ctx.layouts.getOrElse(name, throw new RuntimeException(s"No layouts named $name in ${ctx.layouts}")))

    def toJava(m: Any): Any = m match 
      case sm: Map[_, _] => sm.map(kv => (kv._1, toJava(kv._2))).asJava
      case sl: Iterable[_] => new ArrayList(sl.map( toJava ).asJava.asInstanceOf[Collection[_]])
      case _ => m

    // Library requires mutable maps..
    val mutableProperties = new HMap[String, Object](toJava(ctx.properties).asInstanceOf[JMap[String, Object]])
    val rendered = Template.parse(this.rawCode).render(mutableProperties)
    // We want to render markdown only if next template is html
    val code = if (isHtml || layoutTemplate.exists(!_.isHtml)) rendered else
      val parser: Parser = Parser.builder().build()
      HtmlRenderer.builder(defaultMarkdownOptions).build().render(parser.parse(rendered))

    layoutTemplate match
      case None => ResolvedPage(code, resources ++ ctx.resources)
      case Some(layoutTemplate) =>
        layoutTemplate.resolveInner(ctx.nest(code, file, resources))

object TemplateFile:
  extension (settings: Map[String, Element]):
    private def stringSetting(name: String): Option[String] = settings.getOrElse(name, null).match {
      case List(elem: String) => Some(elem)
      case elem: String => Some(elem)
      case _ => None
    }.map(_.stripPrefix("\"").stripSuffix("\""))
    
    private def listSetting(name: String): Option[List[String]] = settings.getOrElse(name, null).match {
      case elem: List[_] => Some(elem.asInstanceOf[List[String]])
      case elem: String => Some(List(elem))
      case _ => None
    }

  def apply(
    file: File,
    isHtml: Boolean,
    rawCode: String,
    settings: Map[String, Element],
  ): TemplateFile = {
    val name = settings.stringSetting("name").getOrElse(file.getName.stripSuffix(if (isHtml) ".html" else ".md"))

    TemplateFile(
      file = file,
      isHtml,
      rawCode = rawCode,
      settings = settings,
      name = name,
      title = settings.getOrElse("page", settings).asInstanceOf[Map[String, Object]].stringSetting("title").getOrElse(name),
      hasFrame = !settings.stringSetting("hasFrame").contains("false"),
      resources = (settings.listSetting("extraCSS") ++ settings.listSetting("extraJS")).flatten.toList,
      layout = settings.stringSetting("layout")
    )
  }
