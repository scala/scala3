package quotes
import scala.quoted.*

object Quotes_1 {
  def printHello(using Quotes): Expr[Unit] = '{ println("Hello") }
  $printHello // error
}
