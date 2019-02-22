import scala.quoted._

class Foo {
  def a: Expr[Int] = '{1}
  def b: Expr[Int] = '{
    ${ this.a }
  }

  def d: Expr[Expr[Int]] = '{ '{1} }
  def e: Expr[Expr[Int]] = '{
    '{${${this.d}}}
  }

  def foo[T](x: T): T = x

  def f = '{
    ${ foo[this.type](this).a }
  }

  inline def g(): Unit = ${ Foo.impl[this.type](1) }
  inline def h(): Unit = ${ Foo.impl[Any]('this) }
  inline def i(that: Foo): Unit = ${ Foo.impl[that.type](1) }

}

object Foo {
  def impl[T](x: Any): Expr[Unit] = '{}
}
