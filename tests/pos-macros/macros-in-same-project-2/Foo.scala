import scala.quoted._

object Foo {

  def aMacroImplementation(using QuoteContext): Expr[Unit] = '{ println("Hello") }

}
