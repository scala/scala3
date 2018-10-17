package scala.runtime.quoted

import scala.quoted.Exprs.LiftedExpr
import scala.quoted.{Expr, Type}

/** Provides methods to unpickle `Expr` and `Type` trees. */
trait Unpickler {
  private[quoted] def unpickleExpr[T](repr: Unpickler.Pickled, args: Seq[Any]): Expr[T]
  private[quoted] def unpickleType[T](repr: Unpickler.Pickled, args: Seq[Any]): Type[T]
}

object Unpickler {

  /** Representation of pickled trees. For now a List[String],
   *  but it should be changed to some kind of TASTY bundle.
   */
  type Pickled = List[String]

  /** Unpickle `repr` which represents a pickled `Expr` tree,
   *  replacing splice nodes with `args`
   */
  def unpickleExpr[T](repr: Pickled, args: Seq[Any], unpickler: Unpickler): Expr[T] =
    unpickler.unpickleExpr(repr, args)

  /** Lift the `value` to an `Expr` tree.
   *  Values can only be of type Boolean, Byte, Short, Char, Int, Long, Float, Double, Unit, String, Null or Class.
   */
  def liftedExpr[T](value: T): Expr[T] = new LiftedExpr[T](value)

  /** Unpickle `repr` which represents a pickled `Type` tree,
   *  replacing splice nodes with `args`
   */
  def unpickleType[T](repr: Pickled, args: Seq[Any], unpickler: Unpickler): Type[T] =
    unpickler.unpickleType(repr, args)
}
