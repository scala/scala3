import scala.quoted._

class Foo {
 def a(using s: Scope): s.Expr[Int] = '{1}
 def b(using s: Scope): s.Expr[Int] = '{
   ${ this.a }
 }

  def d(using s: Scope): s.Expr[(s2: Scope) ?=> s2.Expr[Int]] = '{ (using q: Scope) => '{1} } // FIXME

 def foo[T](x: T): T = x

 def f(using s: Scope) = '{
   ${ foo[this.type](this).a }
 }

 inline def g(): Unit = ${ Foo.impl[this.type](1) }
 inline def h(): Unit = ${ Foo.impl[Any]('this) }
 // FIXME
//  inline def i(that: Foo): Unit = ${ Foo.impl[that.type](1) }

}

object Foo {
  def impl[T](x: Any)(using s: Scope): s.Expr[Unit] = '{}
}
