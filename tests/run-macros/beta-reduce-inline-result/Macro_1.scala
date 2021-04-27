import scala.quoted.*

object Macros {
  inline def betaReduce[Arg,Result](inline fn : Arg=>Result)(inline arg: Arg): Result =
    ${ betaReduceImpl('{ fn })('{ arg }) }

  def betaReduceImpl[Arg: Type, Result: Type](fn: Expr[Arg=>Result])(arg: Expr[Arg])(using qctx : Quotes): Expr[Result] =
    Expr.betaReduce('{$fn($arg)})

  inline def betaReduceAdd1[Arg](inline fn: Arg=>Int)(inline arg: Arg): Int =
    ${ betaReduceAdd1Impl('{ fn })('{ arg }) }

  def betaReduceAdd1Impl[Arg: Type](fn: Expr[Arg=>Int])(arg: Expr[Arg])(using Quotes): Expr[Int] =
    val app = '{$fn.asInstanceOf[Arg=>Int]($arg)} // FIXME: remove asInstanceOf (workaround for #8612)
    '{ ${ Expr.betaReduce(app) } + 1 }
}

