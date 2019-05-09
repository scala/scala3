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

  implied for Liftable[Boolean] = new PrimitiveLifable
  implied for Liftable[Short] = new PrimitiveLifable
  implied for Liftable[Int] = new PrimitiveLifable
  implied for Liftable[Long] = new PrimitiveLifable
  implied for Liftable[Float] = new PrimitiveLifable
  implied for Liftable[Double] = new PrimitiveLifable
  implied for Liftable[Char] = new PrimitiveLifable
  implied for Liftable[String] = new PrimitiveLifable
  implied ClassIsLiftable[T] for Liftable[Class[T]] = new PrimitiveLifable // FIXME: annonymous implied with type parameter not working

  private class PrimitiveLifable[T] extends Liftable[T] {
    override def toExpr(x: T): Expr[T] = liftedExpr(x)
  }

}
