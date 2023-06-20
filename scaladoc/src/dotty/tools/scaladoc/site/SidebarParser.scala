package dotty.tools.scaladoc
package site

import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.core.`type`.TypeReference;
import scala.jdk.CollectionConverters._
import java.util.Optional
import scala.beans._
import java.nio.file.{Files, Paths}
import scala.io.Source

enum Sidebar:
  case Category(
    title: Option[String],
    indexPath: Option[String],
    nested: List[Sidebar],
    directory: Option[String]
  )
  case Page(title: Option[String], pagePath: String, hidden: Boolean)

object Sidebar:
  case class RawInput(
    @BeanProperty var title: String,
    @BeanProperty var page: String,
    @BeanProperty var index: String,
    @BeanProperty var subsection: JList[RawInput],
    @BeanProperty var directory: String,
    @BooleanBeanProperty var hidden: Boolean
  ):
    def this() = this("", "", "", JList(), "", false)

  private object RawInputTypeRef extends TypeReference[RawInput]

  private def toSidebar(r: RawInput, content: String | java.io.File)(using CompilerContext): Sidebar = r match
    case RawInput(title, page, index, subsection, dir, hidden) if page.nonEmpty && index.isEmpty && subsection.isEmpty() =>
      Sidebar.Page(Option.when(title.nonEmpty)(title), page, hidden)
    case RawInput(title, page, index, subsection, dir, hidden) if page.isEmpty && (!subsection.isEmpty() || !index.isEmpty()) =>
      Sidebar.Category(Option.when(title.nonEmpty)(title), Option.when(index.nonEmpty)(index), subsection.asScala.map(toSidebar(_, content)).toList, Option.when(dir.nonEmpty)(dir))
    case RawInput(title, page, index, subsection, dir, hidden) =>
      if title.isEmpty() && index.isEmpty() then
        val msg = "`title` property is missing for some page."
        report.error(s"$msg\n$schemaMessage")
      else if title.nonEmpty && (page.isEmpty() || index.isEmpty()) then
        val msg = s"Error parsing YAML configuration file: 'index' or 'page' path is missing for title '$title'."
        report.error(s"$msg\n$schemaMessage")
      else
        val msg = "Problem when parsing YAML configuration file."
        report.warning(s"$msg\n$schemaMessage")
      Sidebar.Page(None, page, hidden)

  def schemaMessage: String =
    s"""Static site YAML configuration file should comply with the following description:
      |The root element of static site needs to be <subsection>
      |`title` and `directory` properties are ignored in root subsection.
      |
      |<subsection>:
      |  title: <string> # optional - Default value is file name. Title can be also set using front-matter.
      |  index: <string> # optional - If not provided, default empty index template is generated.
      |  directory: <string> # optional - By default, directory name is title name in kebab case.
      |  subsection: # optional - If not provided, pages are loaded from the index directory
      |    - <subsection> | <page>
      |  # either index or subsection needs to be present
      |<page>:
      |  title: <string> # optional - Default value is file name. Title can be also set using front-matter.
      |  page: <string>
      |  hidden: <boolean> # optional - Default value is false.
      |
      |For more information visit:
      |https://docs.scala-lang.org/scala3/guides/scaladoc/static-site.html""".stripMargin

  def load(content: String | java.io.File)(using CompilerContext): Sidebar.Category =
    import scala.util.Try
    val mapper = ObjectMapper(YAMLFactory())
    def readValue = content match
      case s: String => mapper.readValue(s, RawInputTypeRef)
      case f: java.io.File => mapper.readValue(f, RawInputTypeRef)

    val root: RawInput = Try(readValue)
      .fold(
        { e =>
          report.warn(schemaMessage, e)
          new RawInput()
        },
        identity
      )
    toSidebar(root, content) match
      case c: Sidebar.Category => c
      case _ =>
        report.error(s"Root element is not a subsection.\n$schemaMessage")
        Sidebar.Category(None, None, List.empty, None)
