package dotty.tools.scaladoc

import scala.scalajs.js

@js.native
trait PageEntryJS extends js.Object {
  val n: String = js.native
  val t: String = js.native
  val d: String = js.native
  val l: String = js.native
}

case class PageEntry(
  fullName: String,
  description: String,
  location: String,
  shortName: String,
  tokens: List[String]
)

object PageEntry {
  def apply(jsObj: PageEntryJS): PageEntry = PageEntry(
      jsObj.t,
      jsObj.d,
      jsObj.l,
      jsObj.n.toLowerCase,
      StringUtils.createCamelCaseTokens(jsObj.n)
    )
}
