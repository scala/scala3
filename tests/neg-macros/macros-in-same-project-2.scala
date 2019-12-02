
import scala.quoted._

object Bar {

  myMacro()

  inline def myMacro(): Unit = myMacro2() // error
  inline def myMacro2(): Unit = ${ aMacroImplementation }

  def aMacroImplementation(given QuoteContext): Expr[Unit] = '{}

}
