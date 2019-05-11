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

  implicit for Liftable[Boolean] = new PrimitiveLifable
  implicit for Liftable[Short] = new PrimitiveLifable
  implicit for Liftable[Int] = new PrimitiveLifable
  implicit for Liftable[Long] = new PrimitiveLifable
  implicit for Liftable[Float] = new PrimitiveLifable
  implicit for Liftable[Double] = new PrimitiveLifable
  implicit for Liftable[Char] = new PrimitiveLifable
  implicit for Liftable[String] = new PrimitiveLifable
  implicit ClassIsLiftable[T] for Liftable[Class[T]] = new PrimitiveLifable // FIXME: annonymous implicit with type parameter not working

  private class PrimitiveLifable[T] extends Liftable[T] {
    override def toExpr(x: T): Expr[T] = liftedExpr(x)
  }

}
