import scala.quoted._

object Foo {

  inline def myMacro(): Unit = ${ aMacroImplementation }

  def aMacroImplementation(using QuoteContext): Expr[Unit] =
    throw new NoClassDefFoundError("this.is.not.a.Class")

}
