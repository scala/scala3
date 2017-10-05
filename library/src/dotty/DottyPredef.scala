package dotty

import scala.reflect.ClassTag
import scala.Predef.???

object DottyPredef {

  /** A class for implicit values that can serve as implicit conversions
   *  The implicit resolution algorithm will act as if there existed
   *  the additional implicit definition:
   *
   *    def $implicitConversion[T, U](x: T)(c: ImplicitConverter[T, U]): U = c(x)
   *
   *  However, the presence of this definition would slow down implicit search since
   *  its outermost type matches any pair of types. Therefore, implicit search
   *  contains a special case in `Implicits#discardForView` which emulates the
   *  conversion in a more efficient way.
   *
   *  Note that this is a SAM class - function literals are automatically converted
   *  to `ImplicitConverter` values.
   *
   *  Also note that in bootstrapped dotty, `Predef.<:<` should inherit from
   *  `ImplicitConverter`. This would cut the number of special cases in
   *  `discardForView` from two to one.
   */
  abstract class ImplicitConverter[-T, +U] extends Function1[T, U]

  @inline final def assert(assertion: Boolean, message: => Any): Unit = {
    if (!assertion)
      assertFail(message)
  }

  @inline final def assert(assertion: Boolean): Unit = {
    if (!assertion)
      assertFail()
  }

  final def assertFail(): Unit = throw new java.lang.AssertionError("assertion failed")
  final def assertFail(message: => Any): Unit = throw new java.lang.AssertionError("assertion failed: " + message)

}
