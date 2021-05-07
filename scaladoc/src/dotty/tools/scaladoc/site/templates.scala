package dotty.tools.scaladoc
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
import scala.collection.JavaConverters._

import scala.io.Source

case class RenderingContext(
  properties: Map[String, Object],
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
  settings: Map[String, Object],
  name: String,
  title: String,
  hasFrame: Boolean,
  resources: List[String],
  layout: Option[String],
):
  def isIndexPage() = file.isFile && (file.getName == "index.md" || file.getName == "index.html")

  private[site] def resolveInner(ctx: RenderingContext): ResolvedPage =
    if (ctx.resolving.contains(file.getAbsolutePath))
      throw new RuntimeException(s"Cycle in templates involving $file: ${ctx.resolving}")

    val layoutTemplate = layout.map(name =>
      ctx.layouts.getOrElse(name, throw new RuntimeException(s"No layouts named $name in ${ctx.layouts}")))

    def asJavaElement(o: Object): Object = o match
      case m: Map[_, _] => m.transform {
        case (k: String, v: Object) => asJavaElement(v)
      }.asJava
      case l: List[_] => l.map(x => asJavaElement(x.asInstanceOf[Object])).asJava
      case other => other

    // Library requires mutable maps..
    val mutableProperties = new JHashMap(ctx.properties.transform((_, v) => asJavaElement(v)).asJava)
    val rendered = Template.parse(this.rawCode).render(mutableProperties)
    // We want to render markdown only if next template is html
    val code = if (isHtml || layoutTemplate.exists(!_.isHtml)) rendered else
      val parser: Parser = Parser.builder(defaultMarkdownOptions).build()
      HtmlRenderer.builder(defaultMarkdownOptions).build().render(parser.parse(rendered))
    layoutTemplate match
      case None => ResolvedPage(code, resources ++ ctx.resources)
      case Some(layoutTemplate) =>
        layoutTemplate.resolveInner(ctx.nest(code, file, resources))
