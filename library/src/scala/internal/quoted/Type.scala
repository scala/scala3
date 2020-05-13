package scala.internal.quoted

import scala.quoted._

/** Quoted type (or kind) `T` backed by a tree */
final class Type[Tree](val typeTree: Tree, val scopeId: Int) extends scala.quoted.Type[Any] {
  override def equals(that: Any): Boolean = that match {
    case that: Type[_] => typeTree ==
      // TastyTreeExpr are wrappers around trees, therfore they are equals if their trees are equal.
      // All scopeId should be equal unless two different runs of the compiler created the trees.
      that.typeTree && scopeId == that.scopeId
    case _ => false
  }
  override def hashCode: Int = typeTree.hashCode
  override def toString: String = "'[ ... ]"
}

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
   *  @param qctx the current QuoteContext
   *  @return None if it did not match, `Some(tup)` if it matched where `tup` contains `Type[Ti]``
   */
  def unapply[TypeBindings <: Tuple, Tup <: Tuple](scrutineeType: scala.quoted.Type[_])(using patternType: scala.quoted.Type[_],
        hasTypeSplices: Boolean, qctx: QuoteContext): Option[Tup] = {
    new Matcher.QuoteMatcher[qctx.type].typeTreeMatch(scrutineeType.unseal, patternType.unseal, hasTypeSplices).asInstanceOf[Option[Tup]]
  }

}
