package scala.internal.quoted

import scala.quoted.{Expr, Scope}
import scala.internal.tasty.CompilerInterface.quoteContextWithCompilerInterface

/** Provides methods to unpickle `Expr` and `Type` trees. */
object Unpickler {

  type PickledQuote = List[String]
  type PickledArgs = Seq[Seq[Any] => Any/*(([QCtx <: Scope] =>> QCtx ?=> Expr[Any]) | Type[_])*/]

  /** Unpickle `repr` which represents a pickled `Expr` tree,
   *  replacing splice nodes with `args`
   */
  def unpickleExpr[T](repr: PickledQuote, args: PickledArgs): (s: Scope) ?=> s.Expr[T] = (using s0) =>
    val s = quoteContextWithCompilerInterface(s0)
    s.tasty.unpickleExpr(repr, args).asInstanceOf[s0.Expr[T]]

  /** Unpickle `repr` which represents a pickled `Type` tree,
   *  replacing splice nodes with `args`
   */
  def unpickleType[T](repr: PickledQuote, args: PickledArgs): (s: Scope) ?=> s.Type[T] = (using s0) =>
    val s = quoteContextWithCompilerInterface(s0)
    s.tasty.unpickleType(repr, args).asInstanceOf[s0.Type[T]]

}
