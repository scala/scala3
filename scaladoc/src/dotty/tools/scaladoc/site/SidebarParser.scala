package dotty.tools.scaladoc
package site

import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.core.`type`.TypeReference;
import collection.JavaConverters._
import java.util.Optional
import scala.beans._

enum Sidebar:
  case Root(index: Option[String], pages: List[Sidebar.Child])
  case Category(
    title: Option[String],
    indexPath: Option[String],
    nested: List[Sidebar.Child],
    directory: Option[String]
  )
  case Page(title: Option[String], pagePath: String, hidden: Boolean)

object Sidebar:

  type Child = Category | Page
  case class RawRoot(var rootIndex: String, var pages: JList[RawInput]):
    def this() = this("", JList())

    def setRootIndex(s: String) = rootIndex = s
    def setPages(l: JList[RawInput]) = pages = l

  case class RawInput(
    @BeanProperty var title: String,
    @BeanProperty var page: String,
    @BeanProperty var index: String,
    @BeanProperty var subsection: JList[RawInput],
    @BeanProperty var directory: String,
    @BooleanBeanProperty var hidden: Boolean
  ):
    def this() = this("", "", "", JList(), "", false)

  private object RootTypeRef extends TypeReference[RawRoot]

  private def toSidebar(r: RawInput)(using CompilerContext): Sidebar.Child = r match
    case RawInput(title, page, index, subsection, dir, hidden) if page.nonEmpty && index.isEmpty && subsection.isEmpty() =>
      Sidebar.Page(Option.when(title.nonEmpty)(title), page, hidden)
    case RawInput(title, page, index, subsection, dir, hidden) if page.isEmpty && (!subsection.isEmpty() || !index.isEmpty()) =>
      Sidebar.Category(Option.when(title.nonEmpty)(title), Option.when(index.nonEmpty)(index), subsection.asScala.map(toSidebar).toList, Option.when(dir.nonEmpty)(dir))
    case RawInput(title, page, index, subsection, dir, hidden) =>
      report.error(s"Error parsing YAML configuration file.\n$schemaMessage")
      Sidebar.Page(None, page, hidden)

  private def schemaMessage: String =
    s"""Static site YAML configuration file should comply to the following description:
      |rootIndex: <string> # optional
      |pages:
      |  - <subsection> | <page>
      |
      |<subsection>:
      |  title: <string> # optional
      |  index: <string> # optional
      |  directory: <string> # optional
      |  subsection: # optional
      |    - <subsection> | <page>
      |  # either index or subsection needs to be present
      |<page>:
      |  title: <string> # optional
      |  page: <string>
      |  hidden: <boolean> # optional
      |
      |For more information visit:
      |https://docs.scala-lang.org/scala3/guides/scaladoc/static-site.html
      |""".stripMargin

  def load(content: String | java.io.File)(using CompilerContext): Sidebar.Root =
    import scala.util.Try
    val mapper = ObjectMapper(YAMLFactory())
    def readValue = content match
      case s: String => mapper.readValue(s, RootTypeRef)
      case f: java.io.File => mapper.readValue(f, RootTypeRef)

    val root: RawRoot = Try(readValue)
      .fold(
        { e =>
          report.warn(schemaMessage, e)
          RawRoot("", java.util.Collections.emptyList())
        },
        identity
      )

    val rootIndex: String = root.rootIndex
    val pages: List[Sidebar.Child] = root.pages.asScala.toList.map(toSidebar)
    Sidebar.Root(Option.when(rootIndex.nonEmpty)(rootIndex), pages)
