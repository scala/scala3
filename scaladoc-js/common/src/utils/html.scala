package dotty.tools.scaladoc
package utils

import scala.scalajs.js
import org.scalajs.dom.{html => domhtml, _}

object HTML {
  type TagArg = domhtml.Element | Seq[domhtml.Element | String] | String

  type AttrArg = AppliedAttr | Seq[AppliedAttr]

  case class Tag[T <: domhtml.Element](private val elemFactory: () => T):
    private def textNode(s: String): Text = document.createTextNode(s)

    def apply(tags: TagArg*): T = apply()(tags:_*)
    def apply(first: AttrArg, rest: AttrArg*): T = apply((first +: rest):_*)()
    def apply(attrs: AttrArg*)(tags: TagArg*): T =
      val elem: T = elemFactory()
      def unpackTags(tags: TagArg*): Unit = tags.foreach {
        case e: domhtml.Element => elem.appendChild(e)
        case s: String => elem.appendChild(textNode(s))
        case elemSeq: (Seq[domhtml.Element | String] @unchecked) => unpackTags(elemSeq*)
      }

      def unpackAttributes(attrs: AttrArg*): Unit = attrs.foreach {
        case ("id", id) => elem.id = id
        case ("class", value) => value.split("\\s+").foreach(cls => elem.classList.add(cls))
        case (attr, value) => elem.setAttribute(attr, value)
        case s: Seq[AppliedAttr] => unpackAttributes(s*)
      }

      unpackTags(tags:_*)
      unpackAttributes(attrs:_*)
      elem

  object Tag:
    def apply[T <: domhtml.Element](s: String): Tag[T] =
      Tag[T](() => document.createElement(s).asInstanceOf[T])

  extension (s: String) def escapeReservedTokens: String =
    s.replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")
      .replace("\"", "&quot;")
      .replace("'", "&apos;")

  case class Attr(name: String):
    def :=(value: String): AppliedAttr = (name, value)

  extension (key: String) def :=(value: String): AppliedAttr =
    (key, value)

  opaque type AppliedAttr = (String, String)

  val div = Tag[domhtml.Div]("div")
  val span = Tag[domhtml.Span]("span")
  val a = Tag[domhtml.Anchor]("a")
  val p = Tag[domhtml.Paragraph]("p")
  val h1 = Tag[domhtml.Heading]("h1")
  val h2 = Tag[domhtml.Heading]("h2")
  val h3 = Tag[domhtml.Heading]("h3")
  val h4 = Tag[domhtml.Heading]("h4")
  val h5 = Tag[domhtml.Heading]("h5")
  val h6 = Tag[domhtml.Heading]("h6")
  val dl = Tag[domhtml.DList]("dl")
  val dd = Tag[domhtml.Element]("dd")
  val dt = Tag[domhtml.Element]("dt")
  val svg = Tag[domhtml.Element]("svg")
  val button = Tag[domhtml.Button]("button")
  val input = Tag[domhtml.Input]("input")
  val label = Tag[domhtml.Label]("label")
  val script = Tag[domhtml.Script]("script")
  val link = Tag[domhtml.Link]("link")
  val footer = Tag[domhtml.Element]("footer")
  val htmlelem = Tag[domhtml.Html]("html")
  val head = Tag[domhtml.Head]("head")
  val meta = Tag[domhtml.Element]("meta")
  val main = Tag[domhtml.Element]("main")
  val title = Tag[domhtml.Title]("title")
  val body = Tag[domhtml.Body]("body")
  val nav = Tag[domhtml.Element]("nav")
  val img = Tag[domhtml.Image]("img")
  val ul = Tag[domhtml.UList]("ul")
  val ol = Tag[domhtml.OList]("ol")
  val li = Tag[domhtml.LI]("li")
  val code = Tag[domhtml.Element]("code")
  val pre = Tag[domhtml.Pre]("pre")
  val table = Tag[domhtml.Table]("table")
  val thead = Tag[domhtml.Element]("thead")
  val tbody = Tag[domhtml.Element]("tbody")
  val th = Tag[domhtml.TableCell]("th")
  val tr = Tag[domhtml.TableRow]("tr")
  val td = Tag[domhtml.TableCell]("td")
  val b = Tag[domhtml.Element]("b")
  val i = Tag[domhtml.Element]("i")

  val cls = Attr("class")
  val href = Attr("href")
  val style = Attr("style")
  val id = Attr("id")
  val `type` = Attr("type")
  val placeholder = Attr("placeholder")
  val defer = Attr("defer")
  val src = Attr("src")
  val rel = Attr("rel")
  val charset = Attr("charset")
  val name = Attr("name")
  val content = Attr("content")
  val testId = Attr("data-test-id")
  val alt = Attr("alt")
  val value = Attr("value")
  val onclick=Attr("onclick")
  val titleAttr =Attr("title")
  val onkeyup = Attr("onkeyup")

}