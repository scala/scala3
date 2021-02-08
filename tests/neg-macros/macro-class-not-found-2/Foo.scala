import scala.quoted.*

object Foo {

  inline def myMacro(): Unit = ${ aMacroImplementation }

  def aMacroImplementation(using Quotes): Expr[Unit] =
    throw new NoClassDefFoundError("this.is.not.a.Class")

}
