import scala.quoted.*

object Foo {

  type Macro[X] = Quotes ?=> Expr[X]
  type Tastier[X] = Quotes ?=> X

  implicit inline def foo: String =
    ${fooImpl}

  def fooImpl(using Quotes): Quotes ?=> Tastier[Quotes ?=> Macro[String]] = {
    '{"abc"}
  }

}
