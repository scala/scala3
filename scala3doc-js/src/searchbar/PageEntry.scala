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
  fullName: String,
  description: String,
  location: String,
  shortName: String,
  acronym: Option[String],
)

object PageEntry {
  def apply(jsObj: PageEntryJS): PageEntry = PageEntry(
    jsObj.name,
    jsObj.description,
    jsObj.location,
    jsObj.searchKeys.head.toLowerCase,
    Option.when(jsObj.searchKeys.size > 1)(jsObj.searchKeys.last)
  )
}
