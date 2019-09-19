import scala.quoted.{_, given}

object Foo {

  def aMacroImplementation(given QuoteContext): Expr[Unit] = '{ println("Hello") }

}
