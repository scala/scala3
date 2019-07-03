import scala.quoted._

class Foo {
  def a given QuoteContext: Expr[Int] = '{1}
  def b given QuoteContext: Expr[Int] = '{
    ${ this.a }
  }

  def d given QuoteContext: Expr[Expr[Int]] = '{ '{1} }
  def e given QuoteContext: Expr[Expr[Int]] = '{
    '{${${this.d}}}
  }

  def foo[T](x: T): T = x

  def f given QuoteContext = '{
    ${ foo[this.type](this).a }
  }

  inline def g(): Unit = ${ Foo.impl[this.type](1) }
  inline def h(): Unit = ${ Foo.impl[Any]('this) }
  inline def i(that: Foo): Unit = ${ Foo.impl[that.type](1) }

}

object Foo {
  def impl[T](x: Any) given QuoteContext: Expr[Unit] = '{}
}
