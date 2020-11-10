package scala.internal.quoted

import scala.quoted.{QuoteContext, Expr, Type}
import scala.tasty.reflect._

/** Part of the QuoteContext interface that needs to be implemented by the compiler but is not visible to users */
trait QuoteContextInternal { self: QuoteContext =>

  /** Unpickle `repr` which represents a pickled `Expr` tree,
   *  replacing splice nodes with `holes`
   */
  def unpickleExpr[T](pickled: List[String], fillHole: Int => Seq[Any] => Any): scala.quoted.Expr[T]

  /** Unpickle `repr` which represents a pickled `Type` tree,
   *  replacing splice nodes with `holes`
   */
  def unpickleType[T <: AnyKind](pickled: List[String], fillHole: Int => Seq[Any] => Any): scala.quoted.Type[T]

  val ExprMatch: ExprMatchModule

  trait ExprMatchModule { self: ExprMatch.type =>
    /** Pattern matches an the scrutineeExpr against the patternExpr and returns a tuple
    *  with the matched holes if successful.
    *
    *  Examples:
    *    - `ExprMatch.unapply('{ f(0, myInt) })('{ f(0, myInt) }, _)`
    *       will return `Some(())` (where `()` is a tuple of arity 0)
    *    - `ExprMatch.unapply('{ f(0, myInt) })('{ f(patternHole[Int], patternHole[Int]) }, _)`
    *       will return `Some(Tuple2('{0}, '{ myInt }))`
    *    - `ExprMatch.unapply('{ f(0, "abc") })('{ f(0, patternHole[Int]) }, _)`
    *       will return `None` due to the missmatch of types in the hole
    *
    *  Holes:
    *    - scala.internal.quoted.Patterns.patternHole[T]: hole that matches an expression `x` of type `Expr[U]`
    *                                            if `U <:< T` and returns `x` as part of the match.
    *
    *  @param scrutinee `Expr[Any]` on which we are pattern matching
    *  @param pattern `Expr[Any]` containing the pattern tree
    *  @param hasTypeSplices `Boolean` notify if the pattern has type splices
    *  @param qctx the current QuoteContext
    *  @return None if it did not match, `Some(tup)` if it matched where `tup` contains `Expr[Ti]``
    */
    def unapply[TypeBindings <: Tuple, Tup <: Tuple](scrutinee: Expr[Any])(using pattern: Expr[Any]): Option[Tup]
  }

  val TypeMatch: TypeMatchModule

  trait TypeMatchModule { self: TypeMatch.type =>
    /** Pattern matches an the scrutineeType against the patternType and returns a tuple
     *  with the matched holes if successful.
     *
     *  @param scrutinee `Type[?]` on which we are pattern matching
     *  @param pattern `Type[?]` containing the pattern tree
     *  @param hasTypeSplices `Boolean` notify if the pattern has type splices
     *  @param qctx the current QuoteContext
     *  @return None if it did not match, `Some(tup)` if it matched where `tup` contains `Type[Ti]``
     */
    def unapply[TypeBindings <: Tuple, Tup <: Tuple](scrutinee: Type[?])(using pattern: Type[?]): Option[Tup]
  }
}
