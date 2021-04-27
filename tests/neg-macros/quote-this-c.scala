import scala.quoted.*

class Foo {

  inline def i(): Unit = ${ Foo.impl[Any]('{
    val x: Quotes = ???
    given x.type = x
    'this // error
  }) }

  inline def j(that: Foo): Unit = ${ Foo.impl[Any]('{
    val x: Quotes = ???
    given x.type = x
    'that // error
  }) }

}

object Foo {
  def impl[T](x: Any)(using Quotes): Expr[Unit] = '{}
}
