package scala.internal.quoted

import scala.quoted.{Expr, QuoteContext, Type}

/** Provides methods to unpickle `Expr` and `Type` trees. */
object Unpickler {

  type PickledQuote = List[String]
  type PickledArgs = Seq[Seq[Any] => Any/*(([QCtx <: QuoteContext] =>> QCtx ?=> Expr[Any]) | Type[_])*/]

  /** Unpickle `repr` which represents a pickled `Expr` tree,
   *  replacing splice nodes with `args`
   */
  def unpickleExpr[T](repr: PickledQuote, args: PickledArgs): QuoteContext ?=> Expr[T] =
    summon[QuoteContext].tasty.internal.unpickleExpr(repr, args).asInstanceOf[Expr[T]]

  /** Unpickle `repr` which represents a pickled `Type` tree,
   *  replacing splice nodes with `args`
   */
  def unpickleType[T](repr: PickledQuote, args: PickledArgs): QuoteContext ?=> Type[T] =
    summon[QuoteContext].tasty.internal.unpickleType(repr, args).asInstanceOf[Type[T]]

}
