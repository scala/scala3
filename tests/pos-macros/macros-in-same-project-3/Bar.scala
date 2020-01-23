import scala.quoted._

object Bar {

  inline def myMacro(): Unit = ${ aMacroImplementation }

  def aMacroImplementation with QuoteContext : Expr[Unit] = Foo.hello()

}
