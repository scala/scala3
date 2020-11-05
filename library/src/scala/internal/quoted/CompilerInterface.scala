package scala.internal.quoted

import scala.quoted.QuoteContext
import scala.tasty.reflect._
import scala.internal.quoted.PickledQuote

/** Part of the reflection interface that needs to be implemented by the compiler */
trait CompilerInterface { self: scala.quoted.QuoteContext =>

  import self.reflect._

  /** Unpickle `repr` which represents a pickled `Expr` tree,
   *  replacing splice nodes with `holes`
   */
  def unpickleTerm(pickledQuote: PickledQuote): Term

  /** Unpickle `repr` which represents a pickled `Type` tree,
   *  replacing splice nodes with `holes`
   */
  def unpickleTypeTree(pickledQuote: PickledQuote): TypeTree

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
   *  @param scrutinee `Term` on which we are pattern matching
   *  @param pattern `Term` containing the pattern tree
   *  @return None if it did not match, `Some(tup)` if it matched where `tup` contains `Term``
   */
  def termMatch(scrutinee: Term, pattern: Term): Option[Tuple]

  /** Pattern matches the scrutineeType against the patternType and returns a tuple
   *  with the matched holes if successful.
   *
   *  @param scrutinee `TypeTree` on which we are pattern matching
   *  @param pattern `TypeTree` containing the pattern tree
   *  @return None if it did not match, `Some(tup)` if it matched where `tup` contains `quoted.Type[Ti]``
   */
  def typeTreeMatch(scrutinee: TypeTree, pattern: TypeTree): Option[Tuple]

}


object CompilerInterface {

  private[scala] def quoteContextWithCompilerInterface(qctx: QuoteContext): qctx.type { val reflect: qctx.reflect.type } & CompilerInterface =
    qctx.asInstanceOf[qctx.type { val reflect: qctx.reflect.type } & CompilerInterface]

}
