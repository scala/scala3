import scala.quoted._

object Foo {

  type Macro[X] = QuoteContext ?=> Expr[X]
  type Tastier[X] = QuoteContext ?=> X

  implicit inline def foo: String =
    ${fooImpl}

  def fooImpl(using QuoteContext): QuoteContext ?=> Tastier[QuoteContext ?=> Macro[String]] = {
    '{"abc"}
  }

}
