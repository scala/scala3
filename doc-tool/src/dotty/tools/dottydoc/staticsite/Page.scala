package dotty.tools
package dottydoc
package staticsite


import dotc.util.SourceFile
import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.ext.yaml.front.matter.AbstractYamlFrontMatterVisitor
import java.util.{ Map => JMap, List => JList }
import java.io.{ OutputStreamWriter, BufferedWriter }

import io.VirtualFile
import dotc.core.Contexts.Context
import model.Package
import scala.io.Codec

/** When the YAML front matter cannot be parsed, this exception is thrown */
case class IllegalFrontMatter(message: String) extends Exception(message)

trait Page {
  import scala.collection.JavaConverters._

  /** Full map of includes, from name to contents */
  def includes: Map[String, Include]

  /** `SourceFile` with contents of page */
  def sourceFile: SourceFile

  /** String containing full unexpanded content of page */
  final lazy val content: String = new String(sourceFile.content)

  /** Parameters to page */
  def params: Map[String, AnyRef]

  /** Path to template */
  def path: String

  /** YAML front matter from the top of the file */
  def yaml(implicit ctx: Context): Map[String, AnyRef] = {
    if (_yaml eq null) initFields
    _yaml
  }

  /** HTML generated from page */
  def html(implicit ctx: Context): Option[String] = {
    if (_html eq null) initFields
    _html
  }

  /** First paragraph of page extracted from rendered HTML */
  def firstParagraph(implicit ctx: Context): String = {
    if (_html eq null) initFields

    _html.map { _html =>
      val sb = new StringBuilder
      var pos = 0
      // to handle nested paragraphs in non markdown code
      var open = 0

      while (pos < _html.length - 4) {
        val str = _html.substring(pos, pos + 4)
        val lstr = str.toLowerCase
        sb append str.head

        pos += 1
        if (lstr.contains("<p>"))
          open += 1
        else if (lstr == "</p>") {
          open -= 1
          if (open == 0) {
            pos = Int.MaxValue
            sb append "/p>"
          }
        }
      }

      sb.toString
    }
    .getOrElse("")
  }

  protected def virtualFile(subSource: String): SourceFile = {
    val virtualFile = new VirtualFile(path, path)
    val writer = new BufferedWriter(new OutputStreamWriter(virtualFile.output, "UTF-8"))
    writer.write(subSource)
    writer.close()

    new SourceFile(virtualFile, Codec.UTF8)
  }

  protected[this] var _yaml: Map[String, AnyRef /* String | JList[String] */] = _
  protected[this] var _html: Option[String] = _
  protected[this] def initFields(implicit ctx: Context) = {
    val md = Parser.builder(Site.markdownOptions).build.parse(content)
    val yamlCollector = new AbstractYamlFrontMatterVisitor()
    yamlCollector.visit(md)

    _yaml = updatedYaml {
      yamlCollector
      .getData().asScala
      .toMap
      .transform {
        case (_, xs) if xs.size == 1 =>
          val str = xs.get(0)
          if (str.length > 0 && str.head == '"' && str.last == '"')
            str.substring(1, str.length - 1)
          else str
        case (_, xs) => xs
      }
    }

    // YAML must start with "---" and end in either "---" or "..."
    val withoutYaml = virtualFile(
      if (content.startsWith("---\n")) {
        val str =
          content.linesIterator
          .drop(1)
          .dropWhile(line => line != "---" && line != "...")
          .drop(1).mkString("\n")

        if (str.isEmpty) throw IllegalFrontMatter(content)
        else str
      }
      else content
    )

    // make accessible via "{{ page.title }}" in templates
    val page = Map("page" ->  _yaml.asJava)
    _html = LiquidTemplate(path, withoutYaml).render(params ++ page, includes)
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

class HtmlPage(
  val path: String,
  val sourceFile: SourceFile,
  val params: Map[String, AnyRef],
  val includes: Map[String, Include]
) extends Page

class MarkdownPage(
  val path: String,
  val sourceFile: SourceFile,
  val params: Map[String, AnyRef],
  val includes: Map[String, Include],
  docs: Map[String, Package]
) extends Page {

  override protected[this] def initFields(implicit ctx: Context) = {
    super.initFields
    _html = _html.map { _html =>
      val md = Parser.builder(Site.markdownOptions).build.parse(_html)
      // fix markdown linking
      MarkdownLinkVisitor(md, docs, params)
      MarkdownCodeBlockVisitor(md)
      HtmlRenderer
        .builder(Site.markdownOptions)
        .escapeHtml(false)
        .build()
        .render(md)
    }
  }
}
