import scala.quoted._

object Bar {

  Foo.myMacro() // error

  def hello() given QuoteContext: Expr[Unit] = '{ println("Hello") }
}
