import scala.quoted._

object Foo {

  def aMacroImplementation(using Quotes): Expr[Unit] = '{ println("Hello") }

}
