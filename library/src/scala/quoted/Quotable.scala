package scala.quoted
import scala.math

/** A typeclass for types that can be turned to `quoted.Expr[T]`
 *  without going through an explicit `'(...)` operation.
 */
abstract class Quotable[T] {
  implicit def toExpr(x: T): Expr[T]
}

/** Some base quotable types. To be completed with at least all types
 *  that are valid Scala literals. The actual implementation of these
 *  typed could be in terms of `ast.tpd.Literal`; the test `quotable.scala`
 *  gives an alternative implementation using just the basic staging system.
 */
object Quotable {

  implicit def IntIsQuotable: Quotable[Int] = ???
  implicit def BooleanIsQuotable: Quotable[Boolean] = ???
}
