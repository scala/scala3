package scala.internal.quoted

import scala.quoted._
import scala.internal.quoted.CompilerInterface.quoteContextWithCompilerInterface

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
      throw new scala.internal.quoted.ScopeException("Cannot call `scala.quoted.staging.run(...)` within a macro or another `run(...)`")

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
    val qctx1 = quoteContextWithCompilerInterface(qctx)
    qctx1.typeMatch(scrutineeType, patternType).asInstanceOf[Option[Tup]]
  }


  // TODO generalize following optimizations for all classes without parameters

  def Unit: QuoteContext ?=> quoted.Type[Unit] =
    qctx.reflect.TypeRepr.typeConstructorOf(classOf[Unit]).seal.asInstanceOf[quoted.Type[Unit]]

  def Boolean: QuoteContext ?=> quoted.Type[Boolean] =
    qctx.reflect.TypeRepr.typeConstructorOf(classOf[Boolean]).seal.asInstanceOf[quoted.Type[Boolean]]

  def Byte: QuoteContext ?=> quoted.Type[Byte] =
    qctx.reflect.TypeRepr.typeConstructorOf(classOf[Byte]).seal.asInstanceOf[quoted.Type[Byte]]

  def Char: QuoteContext ?=> quoted.Type[Char] =
    qctx.reflect.TypeRepr.typeConstructorOf(classOf[Char]).seal.asInstanceOf[quoted.Type[Char]]

  def Short: QuoteContext ?=> quoted.Type[Short] =
    qctx.reflect.TypeRepr.typeConstructorOf(classOf[Short]).seal.asInstanceOf[quoted.Type[Short]]

  def Int: QuoteContext ?=> quoted.Type[Int] =
    qctx.reflect.TypeRepr.typeConstructorOf(classOf[Int]).seal.asInstanceOf[quoted.Type[Int]]

  def Long: QuoteContext ?=> quoted.Type[Long] =
    qctx.reflect.TypeRepr.typeConstructorOf(classOf[Long]).seal.asInstanceOf[quoted.Type[Long]]

  def Float: QuoteContext ?=> quoted.Type[Float] =
    qctx.reflect.TypeRepr.typeConstructorOf(classOf[Float]).seal.asInstanceOf[quoted.Type[Float]]

  def Double: QuoteContext ?=> quoted.Type[Double] =
    qctx.reflect.TypeRepr.typeConstructorOf(classOf[Double]).seal.asInstanceOf[quoted.Type[Double]]

}
