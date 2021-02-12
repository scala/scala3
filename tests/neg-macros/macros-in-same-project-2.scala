
import scala.quoted.*

object Bar {

  myMacro() // error

  inline def myMacro(): Unit = myMacro2()
  inline def myMacro2(): Unit = ${ aMacroImplementation }

  def aMacroImplementation(using Quotes): Expr[Unit] = '{}

}
