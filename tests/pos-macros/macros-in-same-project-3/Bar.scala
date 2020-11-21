import scala.quoted._

object Bar {

  inline def myMacro(): Unit = ${ aMacroImplementation }

  def aMacroImplementation(using Quotes): Expr[Unit] = Foo.hello()

}
