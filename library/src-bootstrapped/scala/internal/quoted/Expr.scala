package scala.internal.quoted

import scala.quoted._
import scala.internal.quoted.CompilerInterface.quoteContextWithCompilerInterface

/** An Expr backed by a tree. Only the current compiler trees are allowed.
 *
 *  These expressions are used for arguments of macros. They contain and actual tree
 *  from the program that is being expanded by the macro.
 *
 *  May contain references to code defined outside this Expr instance.
 */
final class Expr[Tree](val tree: Tree, val scopeId: Int) extends scala.quoted.Expr[Any] {
  override def equals(that: Any): Boolean = that match {
    case that: Expr[_] =>
      // Expr are wrappers around trees, therefore they are equals if their trees are equal.
      // All scopeId should be equal unless two different runs of the compiler created the trees.
      tree == that.tree && scopeId == that.scopeId
    case _ => false
  }

  def unseal(using qctx: QuoteContext): qctx.reflect.Term =
    checkScopeId(qctx.hashCode)
    tree.asInstanceOf[qctx.reflect.Term]

  def checkScopeId(expectedScopeId: Int): Unit =
    if expectedScopeId != scopeId then
      throw new scala.internal.quoted.ScopeException("Cannot call `scala.quoted.staging.run(...)` within a macro or another `run(...)`")

  override def hashCode: Int = tree.hashCode
  override def toString: String = "'{ ... }"
}

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
   *  @param scrutineeExpr `Expr[Any]` on which we are pattern matching
   *  @param patternExpr `Expr[Any]` containing the pattern tree
   *  @param hasTypeSplices `Boolean` notify if the pattern has type splices
   *  @param qctx the current QuoteContext
   *  @return None if it did not match, `Some(tup)` if it matched where `tup` contains `Expr[Ti]``
   */
  def unapply[TypeBindings <: Tuple, Tup <: Tuple](scrutineeExpr: scala.quoted.Expr[Any])
        (using patternExpr: scala.quoted.Expr[Any], qctx: QuoteContext): Option[Tup] = {
    val qctx1 = quoteContextWithCompilerInterface(qctx)
    qctx1.exprMatch(scrutineeExpr, patternExpr).asInstanceOf[Option[Tup]]
  }

  /** Returns a null expresssion equivalent to `'{null}` */
  def `null`: QuoteContext ?=> quoted.Expr[Null] = (using qctx) => {
    import qctx.reflect._
    Literal(Constant.Null()).seal.asInstanceOf[quoted.Expr[Null]]
  }

  /** Returns a unit expresssion equivalent to `'{}` or `'{()}` */
  def Unit: QuoteContext ?=> quoted.Expr[Unit] = (using qctx) => {
    import qctx.reflect._
    Literal(Constant.Unit()).seal.asInstanceOf[quoted.Expr[Unit]]
  }

}
