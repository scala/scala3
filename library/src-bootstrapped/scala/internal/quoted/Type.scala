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

  /** View this expression `quoted.Type[T]` as a `TypeTree` */
  def unseal(using qctx: QuoteContext): qctx.reflect.TypeTree =
    checkScopeId(qctx.hashCode)
    typeTree.asInstanceOf[qctx.reflect.TypeTree]

  def checkScopeId(expectedScopeId: Int): Unit =
    if expectedScopeId != scopeId then
      throw new Exception("Cannot call `scala.quoted.staging.run(...)` within a macro or another `run(...)`")

  override def hashCode: Int = typeTree.hashCode
  override def toString: String = "'[ ... ]"
}

object Type {

  /** Pattern matches an the scrutineeType against the patternType and returns a tuple
   *  with the matched holes if successful.
   *
   *  @param scrutineeType `Type[_]` on which we are pattern matching
   *  @param patternType `Type[_]` containing the pattern tree
   *  @param hasTypeSplices `Boolean` notify if the pattern has type splices
   *  @param qctx the current QuoteContext
   *  @return None if it did not match, `Some(tup)` if it matched where `tup` contains `Type[Ti]``
   */
  def unapply[TypeBindings <: Tuple, Tup <: Tuple](scrutineeType: scala.quoted.Type[_])
      (using patternType: scala.quoted.Type[_], qctx: QuoteContext): Option[Tup] = {
    qctx.asInstanceOf[QuoteContextInternal].typeMatch(scrutineeType, patternType).asInstanceOf[Option[Tup]]
  }

}
