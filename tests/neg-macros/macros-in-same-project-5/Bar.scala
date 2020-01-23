import scala.quoted._

object Bar {

  Foo.myMacro() // error

  def aMacroImplementation with QuoteContext : Expr[Unit] = Bar.hello()

  def hello() with QuoteContext : Expr[Unit] = '{ println("Hello") }
}
