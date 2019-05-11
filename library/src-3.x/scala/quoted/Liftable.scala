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

  implicit def BooleanIsLiftable: Liftable[Boolean] = new PrimitiveLifable
  implicit def ShortIsLiftable: Liftable[Short] = new PrimitiveLifable
  implicit def IntIsLiftable: Liftable[Int] = new PrimitiveLifable
  implicit def LongIsLiftable: Liftable[Long] = new PrimitiveLifable
  implicit def FloatIsLiftable: Liftable[Float] = new PrimitiveLifable
  implicit def DoubleIsLiftable: Liftable[Double] = new PrimitiveLifable
  implicit def CharIsLiftable: Liftable[Char] = new PrimitiveLifable
  implicit def StringIsLiftable: Liftable[String] = new PrimitiveLifable
  implicit def ClassIsLiftable[T]: Liftable[Class[T]] = new PrimitiveLifable

  private class PrimitiveLifable[T] extends Liftable[T] {
    override def toExpr(x: T): Expr[T] = liftedExpr(x)
  }

}
