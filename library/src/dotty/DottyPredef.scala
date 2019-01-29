package dotty

import scala.forceInline
import scala.collection.generic.IsTraversableLike

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

  @forceInline final def assert(assertion: => Boolean, message: => Any): Unit = {
    if (!assertion)
      assertFail(message)
  }

  @forceInline final def assert(assertion: => Boolean): Unit = {
    if (!assertion)
      assertFail()
  }

  def assertFail(): Unit = throw new java.lang.AssertionError("assertion failed")
  def assertFail(message: => Any): Unit = throw new java.lang.AssertionError("assertion failed: " + message)

  @forceInline final def implicitly[T](implicit ev: T): T = ev

  @forceInline def locally[T](body: => T): T = body

  /**
   * Retrieve the single value of a type with a unique inhabitant.
   *
   * @example {{{
   * object Foo
   * val foo = valueOf[Foo.type]
   * // foo is Foo.type = Foo
   *
   * val bar = valueOf[23]
   * // bar is 23.type = 23
   * }}}
   * @group utilities
   */
  @forceInline def valueOf[T](implicit vt: ValueOf[T]): T = vt.value

  /**
    * Add an `empty` operation to all collection instances, returning an empty collection
    * of the same type.
    */
  implicit class EmptyOperation[C](c: C)(implicit isTraversable: IsTraversableLike[C]) {
    // Ideally we would use `withFilter`, but it is not defined on `GenTraversableLike`.
    def empty: C = isTraversable.conversion(c).filter(_ => false)
  }

}
