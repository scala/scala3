import scala.quoted._

class Foo {
 def a(using QuoteContext): Expr[Int] = '{1}
 def b(using QuoteContext): Expr[Int] = '{
   ${ this.a }
 }

  def d(using QuoteContext): Expr[QuoteContext ?=> Expr[Int]] = '{ '{1} }

 def foo[T](x: T): T = x

 def f(using QuoteContext) = '{
   ${ foo[this.type](this).a }
 }

 inline def g(): Unit = ${ Foo.impl[this.type](1) }
 inline def h(): Unit = ${ Foo.impl[Any]('this) }
 // FIXME
//  inline def i(that: Foo): Unit = ${ Foo.impl[that.type](1) }

}

object Foo {
  def impl[T](x: Any)(using QuoteContext): Expr[Unit] = '{}
}
