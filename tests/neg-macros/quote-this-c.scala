import scala.quoted._

class Foo {

  inline def i(): Unit = ${ Foo.impl[Any]('{
    val x: Scope = ???
    given x.type = x
    'this // error
  }) }

  inline def j(that: Foo): Unit = ${ Foo.impl[Any]('{
    val x: Scope = ???
    given x.type = x
    'that // error
  }) }

}

object Foo {
  def impl[T](using s: Scope)(x: Any): s.Expr[Unit] = '{}
}
