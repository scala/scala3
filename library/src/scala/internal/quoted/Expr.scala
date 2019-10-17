package scala.internal.quoted

import scala.quoted._

object Expr {

  /** Pattern matches an the scrutineeExpr against the patternExpr and returns a tuple
   *  with the matched holes if successful.
   *
   *  Examples:
   *    - `Matcher.unapply('{ f(0, myInt) })('{ f(0, myInt) }, _)`
   *       will return `Some(())` (where `()` is a tuple of arity 0)
   *    - `Matcher.unapply('{ f(0, myInt) })('{ f(patternHole[Int], patternHole[Int]) }, _)`
   *       will return `Some(Tuple2('{0}, '{ myInt }))`
   *    - `Matcher.unapply('{ f(0, "abc") })('{ f(0, patternHole[Int]) }, _)`
   *       will return `None` due to the missmatch of types in the hole
   *
   *  Holes:
   *    - scala.internal.Quoted.patternHole[T]: hole that matches an expression `x` of type `Expr[U]`
   *                                            if `U <:< T` and returns `x` as part of the match.
   *
   *  @param scrutineeExpr `Expr[_]` on which we are pattern matching
   *  @param patternExpr `Expr[_]` containing the pattern tree
   *  @param hasTypeSplices `Boolean` notify if the pattern has type splices (if so we use a GADT context)
   *  @param qctx the current QuoteContext
   *  @return None if it did not match, `Some(tup)` if it matched where `tup` contains `Expr[Ti]``
   */
  def unapply[TypeBindings <: Tuple, Tup <: Tuple](scrutineeExpr: Expr[_])(implicit patternExpr: Expr[_],
        hasTypeSplices: Boolean, qctx: QuoteContext): Option[Tup] = {
    import qctx.tasty.{_, given}
    new Matcher.QuoteMatcher[qctx.type].termMatch(scrutineeExpr.unseal, patternExpr.unseal, hasTypeSplices).asInstanceOf[Option[Tup]]
  }

}
