
import scala.quoted._

object Bar {

  myMacro() // error

  inline def myMacro(): Unit = ${ aMacroImplementation }

  def aMacroImplementation(given QuoteContext): Expr[Unit] = '{}

}
