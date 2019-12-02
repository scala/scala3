import scala.quoted._

object Foo {

  inline def myMacro(): Unit = ${ aMacroImplementation }

  def aMacroImplementation(given QuoteContext): Expr[Unit] =
    throw new NoClassDefFoundError("this.is.not.a.Class")

}
