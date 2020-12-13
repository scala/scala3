import scala.quoted._

object Foo {

  def hello()(using Quotes): Expr[Unit] = '{ println("Hello") }

}
