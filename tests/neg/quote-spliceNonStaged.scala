package quotes
import scala.quoted.{_, given}

object Quotes_1 {
  def printHello(given QuoteContext): Expr[Unit] = '{ println("Hello") }
  $printHello // error
}
