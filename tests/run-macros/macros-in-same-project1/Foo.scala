import scala.quoted._

object Foo {

  inline def myMacro(): Unit = ${ aMacroImplementation }

  def aMacroImplementation(using Quotes): Expr[Unit] = '{ println("Hello") }

}