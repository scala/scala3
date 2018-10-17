import scala.quoted._

class Foo {
  def a: Staged[Int] = '{1}
  def b: Staged[Int] = '{
    ${ this.a }
  }

  def d: Staged[Expr[Int]] = '{ '{1} }
  def e: Staged[Expr[Int]] = '{
    '{${${this.d}}}
  }

  def foo[T](x: T): T = x

  def f: Staged[Int] = '{
    ${ foo[this.type](this).a }
  }

  inline def g(): Unit = ${ Foo.impl[this.type](1) }
  inline def h(): Unit = ${ Foo.impl[Any]('this) }
  inline def i(that: Foo): Unit = ${ Foo.impl[that.type](1) }

}

object Foo {
  def impl[T](x: Any): Expr[Unit] = '{}
}
