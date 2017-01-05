package dotty.tools
package dottydoc
package staticsite

import dotc.core.Contexts.Context
import dotty.tools.dottydoc.util.syntax._

import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.ext.front.matter.AbstractYamlFrontMatterVisitor
import liqp.{ Template => LiquidTemplate }

class MarkdownPage(fileContents: => String, params: Map[String, AnyRef]) {
  import scala.collection.JavaConverters._

  def yaml(implicit ctx: Context): Map[String, String] = {
    if (_yaml eq null) initFields()
    _yaml
  }

  def html(implicit ctx: Context): String = {
    if (_html eq null) initFields()
    _html
  }

  private[this] var _yaml: Map[String, String] = _
  private[this] var _html: String = _
  private[this] def initFields()(implicit ctx: Context) = {
    val template = LiquidTemplate.parse(fileContents)
    val yamlCollector = new AbstractYamlFrontMatterVisitor()
    val mdParser = Parser.builder(ctx.docbase.markdownOptions).build

    yamlCollector.visit(mdParser.parse(fileContents))

    _yaml = yamlCollector
      .getData().asScala
      .mapValues(_.asScala.headOption.getOrElse(""))
      .toMap

    // make accessible via "{{ page.title }}" in templates
    val page = Map("page" -> _yaml.asJava)
    val renderedTemplate = template.render((page ++ params).asJava)

    _html = HtmlRenderer
      .builder(ctx.docbase.markdownOptions)
      .escapeHtml(false)
      .build()
      .render(mdParser.parse(renderedTemplate))
  }
}
