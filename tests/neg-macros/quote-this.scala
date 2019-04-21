import scala.quoted._

class Foo {

  def f: Unit = '{
    def bar[T](x: T): T = x
    bar[
      this.type  // error
      ] {
      this  // error
    }
  }

  inline def i(): Unit = ${ Foo.impl[Any]('{
    'this // error
  }) }

  inline def j(that: Foo): Unit = ${ Foo.impl[Any]('{
    'that // error
  }) }

}

object Foo {
  def impl[T](x: Any): Expr[Unit] = '{}
}
