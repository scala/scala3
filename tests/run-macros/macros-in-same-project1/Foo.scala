import scala.quoted._

object Foo {

  inline def myMacro(): Unit = ${ aMacroImplementation }

  def aMacroImplementation with QuoteContext : Expr[Unit] = '{ println("Hello") }

}