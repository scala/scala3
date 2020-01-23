import scala.quoted._

object Foo {

  def aMacroImplementation with QuoteContext : Expr[Unit] = '{ println("Hello") }

}
