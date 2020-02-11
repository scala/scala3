import scala.quoted._

object Macros {
  inline def betaReduce[Arg,Result](inline fn : Arg=>Result)(inline arg: Arg): Result =
    ${ betaReduceImpl('{ fn })('{ arg }) }

  def betaReduceImpl[Arg,Result](fn: Expr[Arg=>Result])(arg: Expr[Arg])(using qctx : QuoteContext): Expr[Result] =
    Expr.betaReduce(fn)(arg)

  inline def betaReduceAdd1[Arg](inline fn: Arg=>Int)(inline arg: Arg): Int =
    ${ betaReduceAdd1Impl('{ fn })('{ arg }) }

  def betaReduceAdd1Impl[Arg](fn: Expr[Arg=>Int])(arg: Expr[Arg])(using qctx: QuoteContext): Expr[Int] =
    '{ ${ Expr.betaReduce(fn)(arg) } + 1 }
}

