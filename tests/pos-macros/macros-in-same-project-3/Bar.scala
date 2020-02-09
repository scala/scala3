import scala.quoted._

object Bar {

  inline def myMacro(): Unit = ${ aMacroImplementation }

  def aMacroImplementation(using QuoteContext): Expr[Unit] = Foo.hello()

}
