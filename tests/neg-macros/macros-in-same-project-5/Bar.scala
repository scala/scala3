import scala.quoted._

object Bar {

  Foo.myMacro() // error

  def aMacroImplementation(using s: Scope): s.Expr[Unit] = Bar.hello()

  def hello()(using s: Scope): s.Expr[Unit] = '{ println("Hello") }
}
