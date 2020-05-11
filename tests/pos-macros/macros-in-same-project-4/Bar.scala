import scala.quoted._

object Bar {

  inline def eqMacro(x: Foo, y: Foo): Boolean = ${ eqMacroExpr('x, 'y) }
  def eqMacroExpr(using s: Scope)(x: s.Expr[Foo], y: s.Expr[Foo]): s.Expr[Boolean] = '{ $x == $y }

  inline def plusMacro(x: Foo, y: Foo): Foo = ${ eqPlusExpr('x, 'y) }
  def eqPlusExpr(using s: Scope)(x: s.Expr[Foo], y: s.Expr[Foo]): s.Expr[Foo] = '{ new Foo($x.value + $y.value) }

}
