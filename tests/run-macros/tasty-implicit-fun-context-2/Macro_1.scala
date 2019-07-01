import scala.quoted._
import scala.tasty.Reflection

object Foo {

  type Macro[X] = given QuoteContext => Expr[X]
  type Tastier[X] = given QuoteContext => X

  implicit inline def foo: String =
    ${fooImpl}

  def fooImpl given QuoteContext: given QuoteContext => Tastier[given QuoteContext => Macro[String]] = {
    '{"abc"}
  }

}
