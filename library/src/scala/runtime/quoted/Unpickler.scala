package scala.runtime.quoted

import scala.internal.quoted.{TastyExpr, TastyType}
import scala.quoted.{Expr, QuoteContext, Type}

/** Provides methods to unpickle `Expr` and `Type` trees. */
object Unpickler {

  /** Representation of pickled trees. For now a List[String],
   *  but it should be changed to some kind of TASTY bundle.
   */
  type Pickled = List[String]

  /** Unpickle `repr` which represents a pickled `Expr` tree,
   *  replacing splice nodes with `args`
   */
  def unpickleExpr[T](repr: Pickled, args: Seq[Seq[Any] => ((given QuoteContext => Expr[Any]) | Type[_])]): given QuoteContext => Expr[T] = new TastyExpr[T](repr, args)

  /** Unpickle `repr` which represents a pickled `Type` tree,
   *  replacing splice nodes with `args`
   */
  def unpickleType[T](repr: Pickled, args: Seq[Seq[Any] => Type[_]]): Type[T] = new TastyType[T](repr, args)
}
