package dotty.dokka

import scala.scalajs.js

@js.native
trait PageEntryJS extends js.Object {
  val name: String = js.native
  val description: String = js.native
  val location: String = js.native
  val searchKeys: js.Array[String] = js.native
}

case class PageEntry(
  name: String,
  description: String,
  location: String,
  searchKeys: Array[String]
)

object PageEntry {
  def apply(jsObj: PageEntryJS): PageEntry = PageEntry(
    jsObj.name,
    jsObj.description,
    jsObj.location,
    jsObj.searchKeys.toArray
  )
}
