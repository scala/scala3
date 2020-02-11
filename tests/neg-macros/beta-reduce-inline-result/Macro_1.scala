import scala.quoted._

object Macros {
  inline def betaReduce[Arg,Result](inline fn: Arg=>Result)(inline arg: Arg): Result =
    ${ betaReduceImpl('{ fn })('{ arg }) }

  def betaReduceImpl[Arg,Result](fn: Expr[Arg=>Result])(arg: Expr[Arg])(using qctx: QuoteContext): Expr[Result] =
    Expr.betaReduce(fn)(arg)
}

