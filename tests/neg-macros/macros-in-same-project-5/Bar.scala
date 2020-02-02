import scala.quoted._

object Bar {

  Foo.myMacro() // error

  def aMacroImplementation(using QuoteContext): Expr[Unit] = Bar.hello()

  def hello()(using QuoteContext): Expr[Unit] = '{ println("Hello") }
}
