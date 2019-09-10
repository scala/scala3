import scala.quoted._

object Foo {

  inline def myMacro(): Unit = ${ aMacroImplementation }

  def aMacroImplementation given QuoteContext: Expr[Unit] = Bar.hello()

}
