import scala.quoted.*

object Foo {

  inline def myMacro(): Unit = ${ aMacroImplementation }

  def aMacroImplementation(using Quotes): Expr[Unit] = Bar.hello()

}
