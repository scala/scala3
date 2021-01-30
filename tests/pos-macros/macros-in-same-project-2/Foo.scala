import scala.quoted.*

object Foo {

  def aMacroImplementation(using Quotes): Expr[Unit] = '{ println("Hello") }

}
