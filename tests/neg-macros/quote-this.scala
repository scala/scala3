import scala.quoted._

class Foo {

  def f(using QuoteContext): Unit = '{
    def bar[T](x: T): T = x
    bar[
      this.type  // error
      ] {
      this  // error
    }
  }

  inline def i(): Unit = ${ Foo.impl[Any]('{
    val x: QuoteContext = ???
    given x.type = x
    'this // error
  }) }

  inline def j(that: Foo): Unit = ${ Foo.impl[Any]('{
    val x: QuoteContext = ???
    given x.type = x
    'that // error
  }) }

}

object Foo {
  def impl[T](x: Any)(using QuoteContext): Expr[Unit] = '{}
}
