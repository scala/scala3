import scala.quoted._

object Foo {

  def aMacroImplementation(given QuoteContext): Expr[Unit] = '{ println("Hello") }

}
