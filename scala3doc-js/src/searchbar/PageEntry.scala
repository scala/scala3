package dotty.dokka

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
  acronym: Option[String]
)

object PageEntry {
  private def createAcronym(s: String): Option[String] =
    s.headOption.map(firstLetter => firstLetter.toString ++ s.tail.filter(_.isUpper))

  def apply(jsObj: PageEntryJS): PageEntry = PageEntry(
    jsObj.t,
    jsObj.d,
    jsObj.l,
    jsObj.n.toLowerCase,
    createAcronym(jsObj.n)
  )
}
