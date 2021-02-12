
import scala.quoted.*

object Bar {

  myMacro() // error

  inline def myMacro(): Unit = ${ aMacroImplementation }

  def aMacroImplementation(using Quotes): Expr[Unit] = '{}

}
