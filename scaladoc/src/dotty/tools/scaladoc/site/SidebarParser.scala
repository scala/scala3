package dotty.tools.scaladoc
package site

import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.core.`type`.TypeReference;
import collection.JavaConverters._
import java.util.Optional

enum Sidebar:
  case Category(title: Option[String], indexPath: Option[String], nested: List[Sidebar])
  case Page(title: Option[String], pagePath: String)

object Sidebar:
  case class RawInput(var title: String, var page: String, var index: String, var subsection: JList[RawInput]):
    def this() = this("", "", "", JList())

    def setTitle(t: String) = this.title = t
    def setPage(p: String) = this.page = p
    def setIndex(i: String) = this.index = i
    def setSubsection(s: JList[RawInput]) = this.subsection = s

  type RawInnerTpe = JMap[String, JList[RawInput]]
  private object RawTypeRef extends TypeReference[RawInnerTpe]

  private def toSidebar(r: RawInput): Sidebar = r match
    case RawInput(title, page, index, subsection) if page.nonEmpty && index.isEmpty && subsection.isEmpty() || title == "Blog" =>
      Sidebar.Page(Option.when(title.nonEmpty)(title), page)
    case RawInput(title, page, index, subsection) if page.isEmpty && !subsection.isEmpty() =>
      Sidebar.Category(Option.when(title.nonEmpty)(title), Option.when(index.nonEmpty)(index), subsection.asScala.map(toSidebar).toList)

  def load(content: String): Seq[Sidebar] =
    val mapper = ObjectMapper(YAMLFactory())
    val raw: RawInnerTpe = mapper.readValue(content, RawTypeRef)

    raw.get("sidebar").asScala.toList.map(toSidebar)
