import scala.quoted._

object Foo {

  def hello() given QuoteContext: Expr[Unit] = '{ println("Hello") }

}
