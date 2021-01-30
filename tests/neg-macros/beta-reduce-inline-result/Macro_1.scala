import scala.quoted.*

object Macros {
  inline def betaReduce[Arg,Result](inline fn: Arg=>Result)(inline arg: Arg): Result =
    ${ betaReduceImpl('{ fn })('{ arg }) }

  def betaReduceImpl[Arg: Type, Result: Type](fn: Expr[Arg=>Result])(arg: Expr[Arg])(using Quotes): Expr[Result] =
    Expr.betaReduce('{$fn($arg)})
}

