import scala.quoted.{_, given}

object Foo {

  inline def myMacro(): Unit = ${ aMacroImplementation }

  def aMacroImplementation(given QuoteContext): Expr[Unit] = '{ println("Hello") }

}
