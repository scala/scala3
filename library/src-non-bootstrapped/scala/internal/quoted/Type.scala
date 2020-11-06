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
    if (qctx.hashCode != scopeId)
      throw new scala.internal.quoted.ScopeException("Cannot call `scala.quoted.staging.run(...)` within a macro or another `run(...)`")
    typeTree.asInstanceOf[qctx.reflect.TypeTree]

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
        hasTypeSplices: Boolean, qctx: QuoteContext): Option[Tup] =
    throw new Exception("Non bootstrapped lib")

  def Unit: QuoteContext ?=> quoted.Type[Unit] =
    throw new Exception("Non bootstrapped lib")

  def Boolean: QuoteContext ?=> quoted.Type[Boolean] =
    throw new Exception("Non bootstrapped lib")

  def Byte: QuoteContext ?=> quoted.Type[Byte] =
    throw new Exception("Non bootstrapped lib")

  def Char: QuoteContext ?=> quoted.Type[Char] =
    throw new Exception("Non bootstrapped lib")

  def Short: QuoteContext ?=> quoted.Type[Short] =
    throw new Exception("Non bootstrapped lib")

  def Int: QuoteContext ?=> quoted.Type[Int] =
    throw new Exception("Non bootstrapped lib")

  def Long: QuoteContext ?=> quoted.Type[Long] =
    throw new Exception("Non bootstrapped lib")

  def Float: QuoteContext ?=> quoted.Type[Float] =
    throw new Exception("Non bootstrapped lib")

  def Double: QuoteContext ?=> quoted.Type[Double] =
    throw new Exception("Non bootstrapped lib")

}
