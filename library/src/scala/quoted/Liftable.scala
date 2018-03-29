package scala.quoted

import scala.runtime.quoted.Unpickler.liftedExpr

/** A typeclass for types that can be turned to `quoted.Expr[T]`
 *  without going through an explicit `'(...)` operation.
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
  implicit def BooleanIsLiftable: Liftable[Boolean] = (x: Boolean) => liftedExpr(x)
  implicit def ByteLiftable: Liftable[Byte] = (x: Byte) => liftedExpr(x)
  implicit def CharIsLiftable: Liftable[Char] = (x: Char) => liftedExpr(x)
  implicit def ShortIsLiftable: Liftable[Short] = (x: Short) => liftedExpr(x)
  implicit def IntIsLiftable: Liftable[Int] = (x: Int) => liftedExpr(x)
  implicit def LongIsLiftable: Liftable[Long] = (x: Long) => liftedExpr(x)
  implicit def FloatIsLiftable: Liftable[Float] = (x: Float) => liftedExpr(x)
  implicit def DoubleIsLiftable: Liftable[Double] = (x: Double) => liftedExpr(x)

  implicit def StringIsLiftable: Liftable[String] = (x: String) => liftedExpr(x)

  implicit def ClassIsLiftable[T]: Liftable[Class[T]] = (x: Class[T]) => liftedExpr(x)
}
