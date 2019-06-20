package scala.quoted

package object show {

  def apply[T](expr: given QuoteContext => Expr[T]) given (toolbox: Toolbox): String = run(expr.show.toExpr)

}
