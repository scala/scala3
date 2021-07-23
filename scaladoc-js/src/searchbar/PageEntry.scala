package dotty.tools.scaladoc

import scala.scalajs.js

@js.native
trait PageEntryJS extends js.Object {
  val n: String = js.native
  val t: String = js.native
  val d: String = js.native
  val l: String = js.native
  val k: String = js.native
}

case class PageEntry(
  fullName: String,
  description: String,
  location: String,
  shortName: String,
  kind: String,
  tokens: List[String]
)

case class InkuireMatch(
  prettifiedSignature: String,
  functionName:        String,
  packageLocation:     String,
  pageLocation:        String,
  entryType:           String
)

object PageEntry {
  def apply(jsObj: PageEntryJS): PageEntry = PageEntry(
      jsObj.t,
      jsObj.d,
      jsObj.l,
      jsObj.n.toLowerCase,
      jsObj.k,
      StringUtils.createCamelCaseTokens(jsObj.n)
    )
}
