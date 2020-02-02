import scala.quoted._

object Foo {

  def hello()(using QuoteContext): Expr[Unit] = '{ println("Hello") }

}
