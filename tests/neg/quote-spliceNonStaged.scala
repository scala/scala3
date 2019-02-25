package quotes
import scala.quoted._

object Quotes_1 {
  def printHello: Expr[Unit] = '{ println("Hello") }
  $printHello // error
}
