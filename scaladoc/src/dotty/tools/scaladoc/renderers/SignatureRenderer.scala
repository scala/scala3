package dotty.tools.scaladoc
package renderers

import util.HTML._
import scala.jdk.CollectionConverters._
import java.net.URI
import java.net.URL
import java.util.{List => JList, Set => JSet}
import scala.util.Try
import org.jsoup.Jsoup
import java.nio.file.Paths

// TODO merge it with new renderer
trait SignatureRenderer:
  def currentDri: DRI
  def link(dri: DRI): Option[String]

  def renderElement(e: SignaturePart, modifiers: AppliedAttr*) = renderElementWith(e, modifiers*)

  def renderLink(name: String, dri: DRI, modifiers: AppliedAttr*) =
    renderLinkContent(name, dri, modifiers:_*)

  def unresolvedLink(content: TagArg, modifiers: AppliedAttr*) =
    span(Attr("data-unresolved-link") := "", modifiers)(content)

  def renderLinkContent(content: TagArg, dri: DRI, modifiers: AppliedAttr*) =
    link(dri) match
      case Some(link) => a(href := link, modifiers)(content)
      case _ => unresolvedLink(content, modifiers:_*)

  def renderElementWith(e: SignaturePart, modifiers: AppliedAttr*) = e match
    case Name(name, dri) =>
      val attrs = Seq(Attr("t") := "n") ++ modifiers
      renderLink(name, dri, attrs*)
    case Type(name, Some(dri)) =>
      val attrs = Seq(Attr("t") := "t") ++ modifiers
      renderLink(name, dri, attrs:_*)
    case Type(name, None) => span(Attr("t") := "t")(name)
    case Keyword(name) => span(Attr("t") := "k")(name)
    case Plain(name) => raw(name)
