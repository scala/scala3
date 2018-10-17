package quotes
import scala.quoted._

object Quotes_1 {
  def printHello: Staged[Unit] = '{ println("Hello") }
  $printHello // error
}
