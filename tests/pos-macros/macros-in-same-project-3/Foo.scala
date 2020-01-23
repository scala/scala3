import scala.quoted._

object Foo {

  def hello() with QuoteContext : Expr[Unit] = '{ println("Hello") }

}
