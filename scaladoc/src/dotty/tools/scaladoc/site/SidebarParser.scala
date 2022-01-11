package dotty.tools.scaladoc
package site

import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.core.`type`.TypeReference;
import collection.JavaConverters._
import java.util.Optional

enum Sidebar:
  case Root(index: Option[String], pages: List[Sidebar])
  case Category(title: Option[String], indexPath: Option[String], nested: List[Sidebar], directory: Option[String])
  case Page(title: Option[String], pagePath: String)

object Sidebar:
  case class RawRoot(var rootIndex: String, var pages: JList[RawInput]):
    def this() = this("", JList())

    def setRootIndex(s: String) = rootIndex = s
    def setPages(l: JList[RawInput]) = pages = l

  case class RawInput(var title: String, var page: String, var index: String, var subsection: JList[RawInput], var directory: String):
    def this() = this("", "", "", JList(), "")

    def setTitle(t: String) = this.title = t
    def setPage(p: String) = this.page = p
    def setIndex(i: String) = this.index = i
    def setSubsection(s: JList[RawInput]) = this.subsection = s
    def setDirectory(d: String) = this.directory = d

  private object RootTypeRef extends TypeReference[RawRoot]

  private def toSidebar(r: RawInput): Sidebar = r match
    case RawInput(title, page, index, subsection, dir) if page.nonEmpty && index.isEmpty && subsection.isEmpty() || title == "Blog" =>
      Sidebar.Page(Option.when(title.nonEmpty)(title), page)
    case RawInput(title, page, index, subsection, dir) if page.isEmpty && (!subsection.isEmpty() || !index.isEmpty()) =>
      Sidebar.Category(Option.when(title.nonEmpty)(title), Option.when(index.nonEmpty)(index), subsection.asScala.map(toSidebar).toList, Option.when(dir.nonEmpty)(dir))

  def load(content: String): Sidebar.Root =
    val mapper = ObjectMapper(YAMLFactory())
    val root: RawRoot = mapper.readValue(content, RootTypeRef)

    val rootIndex: String = root.rootIndex
    val pages: List[Sidebar] = root.pages.asScala.toList.map(toSidebar)
    Sidebar.Root(Option.when(rootIndex.nonEmpty)(rootIndex), pages)

  def load(file: java.io.File): Sidebar.Root =
    val mapper = ObjectMapper(YAMLFactory())
    val root: RawRoot = mapper.readValue(file, RootTypeRef)

    val rootIndex: String = root.rootIndex
    val pages: List[Sidebar] = root.pages.asScala.toList.map(toSidebar)
    Sidebar.Root(Option.when(rootIndex.nonEmpty)(rootIndex), pages)
