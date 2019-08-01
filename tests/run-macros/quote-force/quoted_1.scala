import scala.quoted._
import given scala.quoted.autolift._

case class Location(owners: List[String])

object Location {

  implicit inline def location: Location = ${impl}

  def impl given QuoteContext: Expr[Location] = {
    val list = List("a", "b", "c", "d", "e", "f")
    '{new Location(${list})}
  }

}
