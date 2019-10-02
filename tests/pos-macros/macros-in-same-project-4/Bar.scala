import scala.quoted._

object Bar {

  inline def eqMacro(x: Foo, y: Foo): Boolean = ${ eqMacroExpr('x, 'y) }
  def eqMacroExpr(x: Expr[Foo], y: Expr[Foo])(given QuoteContext): Expr[Boolean] = '{ $x == $y }

  inline def plusMacro(x: Foo, y: Foo): Foo = ${ eqPlusExpr('x, 'y) }
  def eqPlusExpr(x: Expr[Foo], y: Expr[Foo])(given QuoteContext): Expr[Foo] = '{ new Foo($x.value + $y.value) }

}
