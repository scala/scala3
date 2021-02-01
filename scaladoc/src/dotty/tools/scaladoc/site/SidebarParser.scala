package dotty.tools.scaladoc
package site

import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.core.`type`.TypeReference;
import collection.JavaConverters._
import java.util.Optional

enum Sidebar:
  val title: String
  case Category(title: String, nested: List[Sidebar])
  case Page(title: String, url: String)

object Sidebar:
  case class RawInput(var title: String,var url: String, var subsection: JList[RawInput]):
    def this() = this("", "", JList())

    def setTitle(t: String) = this.title = t
    def setUrl(u: String) = this.url = u
    def setSubsection(l: JList[RawInput]) = this.subsection = l

  type RawInnerTpe = JMap[String, JList[RawInput]]
  private object RawTypeRef extends TypeReference[RawInnerTpe]

  private def toSidebar(r: RawInput): Sidebar = r match
    case RawInput(title, url, list) if title.nonEmpty && url.nonEmpty && list.isEmpty() =>
      Sidebar.Page(title, url)
    case RawInput(title, url, list) if title.nonEmpty && url.isEmpty && !list.isEmpty() =>
      Sidebar.Category(title, list.asScala.map(toSidebar).toList)

  def load(content: String): Seq[Sidebar] =
    val mapper = ObjectMapper(YAMLFactory())
    val raw: RawInnerTpe = mapper.readValue(content, RawTypeRef)

    raw.get("sidebar").asScala.toList.map(toSidebar)
