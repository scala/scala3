package scala.internal.quoted

import scala.quoted._
import scala.internal.tasty.CompilerInterface.quoteContextWithCompilerInterface

/** An Expression that is pickled in a  */
trait PickledExpr[+T]:
  def unpickle(): QuoteContext ?=> Expr[T]

object PickledExpr:

  def make[T](pickled: List[String], splices: PickledSplices): PickledExpr[T] =
    new PickledExpr[T]:
      def unpickle(): QuoteContext ?=> Expr[T] =
        val bytes = TastyString.unpickle(pickled)
        val qctx = quoteContextWithCompilerInterface(summon[QuoteContext])
        val tree = qctx.reflect.unpickleTerm(bytes, splices)
        new scala.internal.quoted.Expr(tree, qctx.hashCode).asInstanceOf[Expr[T]]
