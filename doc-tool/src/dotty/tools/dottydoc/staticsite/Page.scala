package dotty.tools
package dottydoc
package staticsite

import dotc.config.Printers.dottydoc

import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.ext.front.matter.AbstractYamlFrontMatterVisitor

import java.util.{ Map => JMap, List => JList }

case class IllegalFrontMatter(message: String) extends Exception(message)

trait Page {
  import scala.collection.JavaConverters._

  def includes: Map[String, String]
  def pageContent: String
  def params: Map[String, AnyRef]

  def yaml: Map[String, AnyRef] = {
    if (_yaml eq null) initFields()
    _yaml
  }

  def html: String = {
    if (_html eq null) initFields()
    _html
  }

  protected[this] var _yaml: Map[String, AnyRef /* String | JList[String] */] = _
  protected[this] var _html: String = _
  protected[this] def initFields() = {
    val md = Parser.builder(Site.markdownOptions).build.parse(pageContent)
    val yamlCollector = new AbstractYamlFrontMatterVisitor()
    yamlCollector.visit(md)

    _yaml = updatedYaml {
      yamlCollector
      .getData().asScala
      .mapValues {
        case xs if xs.size == 1 =>
          val str = xs.get(0)
          if (str.length > 0 && str.head == '"' && str.last == '"')
            str.substring(1, str.length - 1)
          else str
        case xs => xs
      }
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
  private def updatedYaml(newYaml: Map[String, AnyRef]): Map[String, AnyRef] =
    params
    .get("page")
    .flatMap {
      case page: Map[String, AnyRef] @unchecked =>
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

  override protected[this] def initFields() = {
    super.initFields()
    val md = Parser.builder(Site.markdownOptions).build.parse(_html)
    // fix markdown linking
    MarkdownLinkVisitor(md)
    _html = HtmlRenderer
      .builder(Site.markdownOptions)
      .escapeHtml(false)
      .build()
      .render(md)
  }
}
