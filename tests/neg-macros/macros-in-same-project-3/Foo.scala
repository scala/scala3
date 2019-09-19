import scala.quoted.{_, given}

object Foo {

  def hello()(given QuoteContext): Expr[Unit] = '{ println("Hello") }

}
