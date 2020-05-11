import scala.quoted._

object Foo {

  def hello()(using s: Scope): s.Expr[Unit] = '{ println("Hello") }

}
