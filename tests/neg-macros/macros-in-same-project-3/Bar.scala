import scala.quoted.{_, given}

object Bar {

  inline def myMacro(): Unit = ${ aMacroImplementation }

  def aMacroImplementation(given QuoteContext): Expr[Unit] = Foo.hello()

}
