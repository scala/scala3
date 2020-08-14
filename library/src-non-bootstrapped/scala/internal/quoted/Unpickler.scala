package scala.internal.quoted

import scala.quoted.{Expr, QuoteContext, Type}

object Unpickler {

  type PickledQuote = List[String]
  type PickledArgs = Seq[Seq[Any] => Any/*(([QCtx <: QuoteContext] =>> QCtx ?=> Expr[Any]) | Type[_])*/]

  def unpickleExpr[T](repr: PickledQuote, args: PickledArgs): QuoteContext ?=> Expr[T] =
    throw new Exception("Non bootstrapped lib")

  def unpickleType[T](repr: PickledQuote, args: PickledArgs): QuoteContext ?=> Type[T] =
    throw new Exception("Non bootstrapped lib")

}
