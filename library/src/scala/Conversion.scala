package scala

import language.experimental.captureChecking
import annotation.internal.preview

/** A class for implicit values that can serve as implicit conversions.
 *  The implicit resolution algorithm will act as if there existed
 *  the additional implicit definition:
 *
 *    def $implicitConversion[T, U](x: T)(c: Conversion[T, U]): U = c(x)
 *
 *  However, the presence of this definition would slow down implicit search since
 *  its outermost type matches any pair of types. Therefore, implicit search
 *  contains a special case in `Implicits#discardForView` which emulates the
 *  conversion in a more efficient way.
 *
 *  Note that this is a SAM class - function literals are automatically converted
 *  to the `Conversion` values.
 *
 *  Also note that in bootstrapped dotty, `Predef.<:<` should inherit from
 *  `Conversion`. This would cut the number of special cases in `discardForView`
 *  from two to one.
 *
 *  The `Conversion` class can also be used to convert explicitly, using
 *  the `convert` extension method.
 *
 *  @tparam T the input type of the conversion (contravariant)
 *  @tparam U the output type of the conversion (covariant)
 */
@java.lang.FunctionalInterface
abstract class Conversion[-T, +U] extends Function1[T, U]:
  self =>
    /** Converts value `x` of type `T` to type `U`.
     *
     *  @param x the value of type `T` to convert to type `U`
     */
    def apply(x: T): U

    extension (x: T)
      /** `x.convert` converts a value `x` of type `T` to type `U`. */
      def convert = this(x)

object Conversion:
  import annotation.experimental

  /** An opaque type alias to declare "into" parameter types that allow implicit conversions
   *  on corresponding arguments. If the expected type of an expression t is into[T], implicit
   *  conversions are tried from the type of `t` to `T`. `into[T]` types are erased to `T`
   *  in all covariant positions of the types of parameter symbols.
   */
  @preview
  opaque type into[+T] >: T = T

  /** Unwraps an `into`. */
  extension [T](x: into[T])
    @preview
    def underlying: T = x
end Conversion
