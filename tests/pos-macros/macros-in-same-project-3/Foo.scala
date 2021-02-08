import scala.quoted.*

object Foo {

  def hello()(using Quotes): Expr[Unit] = '{ println("Hello") }

}
