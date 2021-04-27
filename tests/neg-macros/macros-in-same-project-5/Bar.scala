import scala.quoted.*

object Bar {

  Foo.myMacro() // error

  def aMacroImplementation(using Quotes): Expr[Unit] = Bar.hello()

  def hello()(using Quotes): Expr[Unit] = '{ println("Hello") }
}
