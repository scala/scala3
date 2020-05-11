import scala.quoted._

object Macros {
  inline def betaReduce[Arg,Result](inline fn : Arg=>Result)(inline arg: Arg): Result =
    ${ betaReduceImpl('{ fn })('{ arg }) }

  def betaReduceImpl[Arg,Result](using s: Scope)(fn: s.Expr[Arg=>Result])(arg: s.Expr[Arg])(using s.Type[Arg], s.Type[Result]): s.Expr[Result] =
    Expr.betaReduce('{$fn($arg)})

  inline def betaReduceAdd1[Arg](inline fn: Arg=>Int)(inline arg: Arg): Int =
    ${ betaReduceAdd1Impl('{ fn })('{ arg }) }

  def betaReduceAdd1Impl[Arg](using s: Scope)(fn: s.Expr[Arg=>Int])(arg: s.Expr[Arg])(using s.Type[Arg]): s.Expr[Int] =
    val app = '{$fn.asInstanceOf[Arg=>Int]($arg)} // FIXME: remove asInstanceOf (workaround for #8612)
    '{ ${ Expr.betaReduce(app) } + 1 }
}

