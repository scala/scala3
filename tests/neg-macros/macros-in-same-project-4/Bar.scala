import scala.quoted.{_, given}

object Bar {

  Foo.myMacro() // error

  def hello()(given QuoteContext): Expr[Unit] = '{ println("Hello") }
}
