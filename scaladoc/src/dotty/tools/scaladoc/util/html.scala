package dotty.tools.scaladoc.util

/**
 * This is trivial html renderer using api inspired by ScalaTags
 * It probably could be more efficient but for now on it should be good enough.
 */
object HTML:
  type AttrArg = AppliedAttr | Seq[AppliedAttr]
  type TagArg = AppliedTag | Seq[AppliedTag] | String | Seq[String]

  case class Tag(name: String):
    def apply(tags: TagArg*): AppliedTag = apply()(tags:_*)
    def apply(first: AttrArg, rest: AttrArg*): AppliedTag = apply((first +: rest):_*)()
    def apply(attrs: AttrArg*)(tags: TagArg*): AppliedTag =
      def unpackTags(tags: TagArg*)(using sb: StringBuilder): StringBuilder =
        tags.foreach {
          case t: AppliedTag =>
            sb.append(t)
          case s: String =>
            sb.append(s.escapeReservedTokens)
          case s: Seq[AppliedTag | String] =>
            unpackTags(s:_*)
        }
        sb
      val sb = StringBuilder()
      sb.append(s"<$name")
      attrs.filter(_ != Nil).foreach{
        case s: Seq[AppliedAttr] =>
          s.foreach(sb.append(" ").append)
        case e: AppliedAttr =>
          sb.append(" ").append(e)
      }
      sb.append(">")
      unpackTags(tags:_*)(using sb)
      sb.append(s"</$name>")
      sb

  extension (s: String) def escapeReservedTokens: String =
    s.replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")
      .replace("\"", "&quot;")
      .replace("'", "&apos;")

  case class Attr(name: String):
    def :=(value: String): AppliedAttr = new AppliedAttr(s"""$name="$value"""")

  opaque type AppliedTag = StringBuilder

  opaque type AppliedAttr = String

  val div = Tag("div")
  val span = Tag("span")
  val a = Tag("a")
  val p = Tag("p")
  val h1 = Tag("h1")
  val h2 = Tag("h2")
  val h3 = Tag("h3")
  val h4 = Tag("h4")
  val h5 = Tag("h5")
  val h6 = Tag("h6")
  val dl = Tag("dl")
  val dd = Tag("dd")
  val dt = Tag("dt")
  val svg = Tag("svg")
  val button = Tag("button")
  val input = Tag("input")
  val label = Tag("label")
  val script = Tag("script")
  val link = Tag("link")
  val footer = Tag("footer")
  val html = Tag("html")
  val head = Tag("head")
  val meta = Tag("meta")
  val main = Tag("main")
  val title = Tag("title")
  val body = Tag("body")
  val nav = Tag("nav")
  val img = Tag("img")
  val ul = Tag("ul")
  val ol = Tag("ol")
  val li = Tag("li")
  val code = Tag("code")
  val pre = Tag("pre")
  val table = Tag("table")
  val thead = Tag("thead")
  val tbody = Tag("tbody")
  val th = Tag("th")
  val tr = Tag("tr")
  val td = Tag("td")

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

  def raw(content: String): AppliedTag = new AppliedTag(content)
  def raw(content: StringBuilder): AppliedTag = content

  def text(content: String) = content.escapeReservedTokens

  val hr = raw("<hr/>")
