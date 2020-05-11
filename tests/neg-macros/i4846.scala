import scala.quoted._

object Test {
  inline def foo(inline x: Int): Int = ${
    fooImpl(
      x, // error
      'x,
      '{ (s: Scope) ?=> 'x }, // error
      '{ (s: Scope) ?=> '{ (s: Scope) ?=> 'x } } // error
    )
  }
  def fooImpl(using s: Scope)(a: Int, b: s.Expr[Int], c: s.Expr[(s2: Scope) ?=> s2.Expr[Int]], d: s.Expr[(s2: Scope) ?=> s2.Expr[(s3: Scope) ?=> s3.Expr[Int]]]): s.Expr[Int] = ???
}
