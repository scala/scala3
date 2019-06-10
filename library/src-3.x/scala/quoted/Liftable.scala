package scala.quoted

import scala.runtime.quoted.Unpickler.liftedExpr

/** A typeclass for types that can be turned to `quoted.Expr[T]`
 *  without going through an explicit `'{...}` operation.
 */
abstract class Liftable[T] {
  def toExpr(x: T): Expr[T]
}

/** Some liftable base types. To be completed with at least all types
 *  that are valid Scala literals. The actual implementation of these
 *  typed could be in terms of `ast.tpd.Literal`; the test `quotable.scala`
 *  gives an alternative implementation using just the basic staging system.
 */
object Liftable {

  implicit val Liftable_Boolean_delegate: Liftable[Boolean] = new PrimitiveLiftable
  implicit val Liftable_Short_delegate: Liftable[Short] = new PrimitiveLiftable
  implicit val Liftable_Int_delegate: Liftable[Int] = new PrimitiveLiftable
  implicit val Liftable_Long_delegate: Liftable[Long] = new PrimitiveLiftable
  implicit val Liftable_Float_delegate: Liftable[Float] = new PrimitiveLiftable
  implicit val Liftable_Double_delegate: Liftable[Double] = new PrimitiveLiftable
  implicit val Liftable_Char_delegate: Liftable[Char] = new PrimitiveLiftable
  implicit val Liftable_String_delegate: Liftable[String] = new PrimitiveLiftable
  implicit def ClassIsLiftable[T]: Liftable[Class[T]] = new PrimitiveLiftable

  private class PrimitiveLiftable[T] extends Liftable[T] {
    override def toExpr(x: T): Expr[T] = liftedExpr(x)
  }

}
