package scala.internal.quoted

import scala.quoted._
import scala.internal.tasty.CompilerInterface.quoteContextWithCompilerInterface

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
   *  @param hasTypeSplices `Boolean` notify if the pattern has type splices (if so we use a GADT context)
   *  @param s the current Scope
   *  @return None if it did not match, `Some(tup)` if it matched where `tup` contains `Expr[Ti]``
   */
  def unapply[TypeBindings <: Tuple, Tup <: Tuple](using s: Quotes)(scrutineeExpr: s.Expr[Any])(using patternExpr: s.Expr[Any],
        hasTypeSplices: Boolean): Option[Tup] =
    s.tasty.termMatch(scrutineeExpr, patternExpr, hasTypeSplices).asInstanceOf[Option[Tup]]

  /** Returns a null expresssion equivalent to `'{null}` */
  def `null`: (s: Scope) ?=> s.Expr[Null] = s ?=> {
    import s.tasty._
    Literal(Constant(null)).seal.asInstanceOf[s.Expr[Null]]
  }

  /** Returns a unit expresssion equivalent to `'{}` or `'{()}` */
  def Unit: (s: Scope) ?=> s.Expr[Unit] = s ?=> {
    import s.tasty._
    Literal(Constant(())).seal.asInstanceOf[s.Expr[Unit]]
  }

  def liftBoolean[T <: Boolean](x: T): (s: Scope) ?=> s.Expr[T] = quoted.Expr(x)
  def liftByte[T <: Byte](x: T): (s: Scope) ?=> s.Expr[T] = quoted.Expr(x)
  def liftShort[T <: Short](x: T): (s: Scope) ?=> s.Expr[T] = quoted.Expr(x)
  def liftInt[T <: Int](x: T): (s: Scope) ?=> s.Expr[T] = quoted.Expr(x)
  def liftLong[T <: Long](x: T): (s: Scope) ?=> s.Expr[T] = quoted.Expr(x)
  def liftFloat[T <: Float](x: T): (s: Scope) ?=> s.Expr[T] = quoted.Expr(x)
  def liftDouble[T <: Double](x: T): (s: Scope) ?=> s.Expr[T] = quoted.Expr(x)
  def liftChar[T <: Char](x: T): (s: Scope) ?=> s.Expr[T] = quoted.Expr(x)
  def liftString[T <: String](x: T): (s: Scope) ?=> s.Expr[T] = quoted.Expr(x)

}
