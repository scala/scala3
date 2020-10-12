package scala.internal.quoted

import scala.quoted.{Expr, QuoteContext, Type}
import scala.internal.tasty.CompilerInterface.quoteContextWithCompilerInterface

/** Provides methods to unpickle `Expr` and `Type` trees. */
object Unpickler {

  type PickledQuote = List[String]
  type PickledArgs = Seq[Seq[Any] => Any/*(([QCtx <: QuoteContext] =>> QCtx ?=> Expr[Any]) | Type[_])*/]

  /** Unpickle `repr` which represents a pickled `Expr` tree,
   *  replacing splice nodes with `args`
   */
  def unpickleExpr[T](repr: PickledQuote, args: PickledArgs): QuoteContext ?=> Expr[T] =
    val qctx = quoteContextWithCompilerInterface(summon[QuoteContext])
    val tree = qctx.reflect.unpickleExpr(repr, args)
    new scala.internal.quoted.Expr(tree, qctx.hashCode).asInstanceOf[Expr[T]]

  /** Unpickle `repr` which represents a pickled `Type` tree,
   *  replacing splice nodes with `args`
   */
  def unpickleType[T](repr: PickledQuote, args: PickledArgs): QuoteContext ?=> Type[T] =
    val qctx = quoteContextWithCompilerInterface(summon[QuoteContext])
    val tree = qctx.reflect.unpickleType(repr, args)
    new scala.internal.quoted.Type(tree, qctx.hashCode).asInstanceOf[Type[T]]

}
