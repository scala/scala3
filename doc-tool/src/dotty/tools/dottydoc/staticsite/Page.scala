package dotty.tools
package dottydoc
package staticsite

import dotc.core.Contexts.Context
import dotty.tools.dottydoc.util.syntax._
import dotc.config.Printers.dottydoc

import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.ext.front.matter.AbstractYamlFrontMatterVisitor
import _root_.java.util.{ Map => JMap }

case class IllegalFrontMatter(message: String) extends Exception(message)

trait Page {
  import scala.collection.JavaConverters._

  def includes: Map[String, String]
  def pageContent: String
  def params: Map[String, AnyRef]

  def yaml(implicit ctx: Context): Map[String, String] = {
    if (_yaml eq null) initFields()
    _yaml
  }

  def html(implicit ctx: Context): String = {
    if (_html eq null) initFields()
    _html
  }

  protected[this] var _yaml: Map[String, String] = _
  protected[this] var _html: String = _
  protected[this] def initFields()(implicit ctx: Context) = {
    val md = Parser.builder(ctx.docbase.markdownOptions).build.parse(pageContent)
    val yamlCollector = new AbstractYamlFrontMatterVisitor()
    yamlCollector.visit(md)

    _yaml = updatedYaml {
      yamlCollector
      .getData().asScala
      .mapValues(_.asScala.headOption.getOrElse(""))
      .toMap
    }

    // YAML must start with "---" and end in either "---" or "..."
    val withoutYaml =
      if (pageContent.startsWith("---\n")) {
        val str =
          pageContent.lines
          .drop(1)
          .dropWhile(line => line != "---" && line != "...")
          .drop(1).mkString("\n")

        if (str.isEmpty) throw IllegalFrontMatter(pageContent)
        else str
      }
      else pageContent

    // make accessible via "{{ page.title }}" in templates
    val page = Map("page" ->  _yaml.asJava)
    _html = LiquidTemplate(withoutYaml).render(params ++ page, includes)
  }

  /** Takes "page" from `params` map in case this is a second expansion, and
    * removes "layout" from the parameters if it exists. We don't want to
    * preserve the layout from the previously expanded template
    */
  private def updatedYaml(newYaml: Map[String, String]): Map[String, String] =
    params
    .get("page")
    .flatMap {
      case page: Map[String, String] @unchecked =>
        Some(page - "layout" ++ newYaml)
      case _ => None
    }
    .getOrElse(newYaml)
}

class HtmlPage(fileContents: => String, val params: Map[String, AnyRef], val includes: Map[String, String]) extends Page {
  lazy val pageContent = fileContents
}

class MarkdownPage(fileContents: => String, val params: Map[String, AnyRef], val includes: Map[String, String]) extends Page {
  lazy val pageContent = fileContents

  override protected[this] def initFields()(implicit ctx: Context) = {
    super.initFields()
    val md = Parser.builder(ctx.docbase.markdownOptions).build.parse(_html)
    // fix markdown linking
    MarkdownLinkVisitor(md)
    _html = HtmlRenderer
      .builder(ctx.docbase.markdownOptions)
      .escapeHtml(false)
      .build()
      .render(md)
  }
}
