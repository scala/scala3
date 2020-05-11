import scala.quoted._

object Macros {
  inline def betaReduce[Arg,Result](inline fn: Arg=>Result)(inline arg: Arg): Result =
    ${ betaReduceImpl('{ fn })('{ arg }) }

  def betaReduceImpl[Arg,Result](using s: Scope)(fn: s.Expr[Arg=>Result])(arg: s.Expr[Arg])(using s.Type[Arg], s.Type[Result]): s.Expr[Result] =
    Expr.betaReduce('{$fn($arg)})
}

