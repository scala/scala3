import scala.quoted.*

class Foo {
 def a(using Quotes): Expr[Int] = '{1}
 def b(using Quotes): Expr[Int] = '{
   ${ this.a }
 }

  def d(using Quotes): Expr[Quotes ?=> Expr[Int]] = '{ '{1} }

 def foo[T](x: T): T = x

 def f(using Quotes) = '{
   ${ foo[this.type](this).a }
 }

 inline def g(): Unit = ${ Foo.impl[this.type](1) }
 inline def h(): Unit = ${ Foo.impl[Any]('this) }
 // FIXME
//  inline def i(that: Foo): Unit = ${ Foo.impl[that.type](1) }

}

object Foo {
  def impl[T](x: Any)(using Quotes): Expr[Unit] = '{}
}
