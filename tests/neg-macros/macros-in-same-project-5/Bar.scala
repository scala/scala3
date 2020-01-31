import scala.quoted._

object Bar {

  Foo.myMacro() // error

  def aMacroImplementation(given QuoteContext): Expr[Unit] = Bar.hello()

  def hello()(given QuoteContext): Expr[Unit] = '{ println("Hello") }
}
