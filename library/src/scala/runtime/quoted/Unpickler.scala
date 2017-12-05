package scala.runtime.quoted

import scala.quoted._

/** Provides methods to unpickle `Expr` and `Type` trees. */
object Unpickler {

  /** Representation of pickled trees. For now it's String, but it
   *  should be changed to some kind of TASTY bundle.
   */
  type Pickled = String

  /** Unpickle `repr` which represents a pickled `Expr` tree,
   *  replacing splice nodes with `args`
   */
  def unpickleExpr[T](repr: Pickled, args: Seq[Any]): Expr[T] = new TastyExpr[T](repr, args)

  /** Unpickle `repr` which represents a pickled `Type` tree,
   *  replacing splice nodes with `args`
   */
  def unpickleType[T](repr: Pickled, args: Seq[Any]): Type[T] = new TastyType[T](repr, args)
}
