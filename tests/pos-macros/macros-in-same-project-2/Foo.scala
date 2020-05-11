import scala.quoted._

object Foo {

  def aMacroImplementation(using s: Scope): s.Expr[Unit] = '{ println("Hello") }

}
