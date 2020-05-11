package scala.internal.quoted

import scala.quoted._
import scala.internal.tasty.CompilerInterface.quoteContextWithCompilerInterface

object Type {

  /** Pattern matches an the scrutineeType against the patternType and returns a tuple
   *  with the matched holes if successful.
   *
   *  Holes:
   *    - scala.internal.Quoted.patternHole[T]: hole that matches an expression `x` of type `Type[U]`
   *                                            if `U <:< T` and returns `x` as part of the match.
   *
   *  @param scrutineeType `Type[_]` on which we are pattern matching
   *  @param patternType `Type[_]` containing the pattern tree
   *  @param hasTypeSplices `Boolean` notify if the pattern has type splices (if so we use a GADT context)
   *  @param s the current Scope
   *  @return None if it did not match, `Some(tup)` if it matched where `tup` contains `Type[Ti]``
   */
  def unapply[TypeBindings <: Tuple, Tup <: Tuple, T, U](using s: Scope)(scrutineeType: s.Type[T])(using patternType: s.Type[U],
        hasTypeSplices: Boolean): Option[Tup] = {
    // TODO facrtor out
    val s1 = quoteContextWithCompilerInterface(s)
    def withWithMatcherState[T](hasTypeSplices: Boolean)(body: (s2: s.Nested) ?=> T) = {
      val s2 = if hasTypeSplices then s1.tasty.Constraints_context else s1
      body(using s2.asInstanceOf[s.Nested])
    }
    withWithMatcherState(hasTypeSplices) {
      new Matcher.QuoteMatcher[scope.type].typeTreeMatch(scrutineeType, patternType, hasTypeSplices).asInstanceOf[Option[Tup]]
    }
  }

  def Unit(using s: Scope): s.Type[Unit] =
    val s1 = quoteContextWithCompilerInterface(s)
    s1.tasty.Definitions_UnitType.seal.get.asInstanceOf[s.Type[Unit]]

  def Boolean(using s: Scope): s.Type[Boolean] =
    val s1 = quoteContextWithCompilerInterface(s)
    s1.tasty.Definitions_BooleanType.seal.get.asInstanceOf[s.Type[Boolean]]

  def Byte(using s: Scope): s.Type[Byte] =
    val s1 = quoteContextWithCompilerInterface(s)
    s1.tasty.Definitions_ByteType.seal.get.asInstanceOf[s.Type[Byte]]

  def Char(using s: Scope): s.Type[Char] =
    val s1 = quoteContextWithCompilerInterface(s)
    s1.tasty.Definitions_CharType.seal.get.asInstanceOf[s.Type[Char]]

  def Short(using s: Scope): s.Type[Short] =
    val s1 = quoteContextWithCompilerInterface(s)
    s1.tasty.Definitions_ShortType.seal.get.asInstanceOf[s.Type[Short]]

  def Int(using s: Scope): s.Type[Int] =
    val s1 = quoteContextWithCompilerInterface(s)
    s1.tasty.Definitions_IntType.seal.get.asInstanceOf[s.Type[Int]]

  def Long(using s: Scope): s.Type[Long] =
    val s1 = quoteContextWithCompilerInterface(s)
    s1.tasty.Definitions_LongType.seal.get.asInstanceOf[s.Type[Long]]

  def Float(using s: Scope): s.Type[Float] =
    val s1 = quoteContextWithCompilerInterface(s)
    s1.tasty.Definitions_FloatType.seal.get.asInstanceOf[s.Type[Float]]

  def Double(using s: Scope): s.Type[Double] =
    val s1 = quoteContextWithCompilerInterface(s)
    s1.tasty.Definitions_DoubleType.seal.get.asInstanceOf[s.Type[Double]]

}
