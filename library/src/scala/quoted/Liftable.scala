package scala.quoted

/** A typeclass for types that can be turned to `quoted.Expr[T]`
 *  without going through an explicit `'(...)` operation.
 */
abstract class Liftable[T] {
  implicit def toExpr(x: T): Expr[T]
}

/** Some liftable base types. To be completed with at least all types
 *  that are valid Scala literals. The actual implementation of these
 *  typed could be in terms of `ast.tpd.Literal`; the test `quotable.scala`
 *  gives an alternative implementation using just the basic staging system.
 */
object Liftable {
  implicit def BooleanIsLiftable: Liftable[Boolean] = (x: Boolean) => new ValueExpr(x)
  implicit def ByteLiftable: Liftable[Byte] = (x: Byte) => new ValueExpr(x)
  implicit def CharIsLiftable: Liftable[Char] = (x: Char) => new ValueExpr(x)
  implicit def ShortIsLiftable: Liftable[Short] = (x: Short) => new ValueExpr(x)
  implicit def IntIsLiftable: Liftable[Int] = (x: Int) => new ValueExpr(x)
  implicit def LongIsLiftable: Liftable[Long] = (x: Long) => new ValueExpr(x)
  implicit def FloatIsLiftable: Liftable[Float] = (x: Float) => new ValueExpr(x)
  implicit def DoubleIsLiftable: Liftable[Double] = (x: Double) => new ValueExpr(x)
}
