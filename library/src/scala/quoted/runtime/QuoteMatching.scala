package scala.quoted.runtime

import language.experimental.captureChecking

import scala.quoted.{Expr, Type}

/** Part of the `Quotes` interface that needs to be implemented by the compiler but is not visible to users. */
trait QuoteMatching:

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
     *       will return `None` due to the mismatch of types in the hole
     *
     *  Holes:
     *    - scala.quoted.runtime.Patterns.patternHole[T]: hole that matches an expression `x` of type `Expr[U]`
     *                                            if `U <:< T` and returns `x` as part of the match.
     *
     *  @tparam TypeBindings a kind-level list (`KList`) encoding the type variables bound in the pattern during matching
     *  @tparam Tup the tuple type containing the types of the matched expression holes, where each element is an `Expr[Ti]`
     *  @param scrutinee `Expr[Any]` on which we are pattern matching
     *  @param pattern `Expr[Any]` containing the pattern tree
     *  @return None if it did not match, `Some(tup)` if it matched where `tup` contains `Expr[Ti]``
     */
    def unapply[TypeBindings, Tup <: Tuple](scrutinee: Expr[Any])(using pattern: Expr[Any]): Option[Tup]
  }

  val TypeMatch: TypeMatchModule

  trait TypeMatchModule { self: TypeMatch.type =>
    /** Pattern matches an the scrutineeType against the patternType and returns a tuple
     *  with the matched holes if successful.
     *
     *  @tparam TypeBindings a kind-level list (`KList`) encoding the type variables bound in the pattern during matching
     *  @tparam Tup the tuple type containing the types of the matched type holes, where each element is a `Type[Ti]`
     *  @param scrutinee `Type[?]` on which we are pattern matching
     *  @param pattern `Type[?]` containing the pattern tree
     *  @return None if it did not match, `Some(tup)` if it matched where `tup` contains `Type[Ti]``
     */
    def unapply[TypeBindings, Tup <: Tuple](scrutinee: Type[?])(using pattern: Type[?]): Option[Tup]
  }

object QuoteMatching:
  type KList
  type KCons[+H <: AnyKind, +T <: KList] <: KList
  type KNil <: KList
