package scala.internal.quoted

import scala.quoted.QuoteContext
import scala.tasty.reflect._
import scala.internal.quoted.PickledQuote

/** Part of the QuoteContext interface that needs to be implemented by the compiler but is not visible to users */
trait QuoteContextInternal { self: scala.quoted.QuoteContext =>

  /** Unpickle `repr` which represents a pickled `Expr` tree,
   *  replacing splice nodes with `holes`
   */
  def unpickleExpr[T](pickledQuote: PickledQuote): scala.quoted.Expr[T]

  /** Unpickle `repr` which represents a pickled `Type` tree,
   *  replacing splice nodes with `holes`
   */
  def unpickleType[T <: AnyKind](pickledQuote: PickledQuote): scala.quoted.Type[T]

  /** Pattern matches the scrutinee against the pattern and returns a tuple
   *  with the matched holes if successful.
   *
   *  Examples:
   *    - `termMatch(< f(0, myInt) >, < f(0, myInt) >)`
   *       will return `Some(())` (where `()` is a tuple of arity 0)
   *    - `termMatch(< f(0, myInt) >, < f(patternHole[Int], patternHole[Int]) >)`
   *       will return `Some(Tuple2('{0}, '{ myInt }))`
   *    - `termMatch(< f(0, "abc") >, < f(0, patternHole[Int]) >)`
   *       will return `None` due to the missmatch of types in the hole
   *
   *  Holes:
   *    - scala.internal.Quoted.patternHole[T]: hole that matches an expression `x` of type `Expr[U]`
   *                                            if `U <:< T` and returns `x` as part of the match.
   *
   *  @param scrutinee `Expr` on which we are pattern matching
   *  @param pattern `Expr` containing the pattern tree
   *  @return None if it did not match, `Some(tup)` if it matched where `tup` contains `Term``
   */
  def exprMatch(scrutinee: scala.quoted.Expr[Any], pattern: scala.quoted.Expr[Any]): Option[Tuple]

  /** Pattern matches the scrutineeType against the patternType and returns a tuple
   *  with the matched holes if successful.
   *
   *  @param scrutinee `Type` on which we are pattern matching
   *  @param pattern `Type` containing the pattern tree
   *  @return None if it did not match, `Some(tup)` if it matched where `tup` contains `scala.quoted.Type[Ti]``
   */
  def typeMatch(scrutinee: scala.quoted.Type[?], pattern: scala.quoted.Type[?]): Option[Tuple]

}
