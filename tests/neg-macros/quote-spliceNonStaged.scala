package quotes
import scala.quoted._

object Quotes_1 {
  def printHello(using s: Scope): s.Expr[Unit] = '{ println("Hello") }
  $printHello // error
}
