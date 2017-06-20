package dotty

import scala.reflect.ClassTag
import scala.Predef.???
import scala.collection.Seq

/** unimplemented implicit for TypeTag */
object DottyPredef {

  /** A fall-back implicit to compare values of any types.
   *  The compiler will restrict implicit instances of `eqAny`. An instance
   *  `eqAny[T, U]` is _valid_ if `T <: U` or `U <: T` or both `T` and `U` are
   *  Eq-free. A type `S` is Eq-free if there is no implicit instance of `Eq[S, S]`.
   *  An implicit search will fail instead of returning an invalid `eqAny` instance.
   *  The method is here instead of the `Eq` object so that it can be disabled.
   */
  implicit def eqAny[L, R]: Eq[L, R] = Eq

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
}
