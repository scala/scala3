package quotes
import scala.quoted._

object Quotes_1 {
  def printHello(using Quotes): Expr[Unit] = '{ println("Hello") }
  $printHello // error
}
