import scala.math.Equiv

object Test {
  def foo[T] =
    new Ordering[T] {
      def compare(x: T, y: T): Int =
        // If we don't disallow implicit conversions to AnyRef, this succeeds with a conversion to
        // mkOrderingOps (https://github.com/scala/scala3/blob/7d4833b619d31ea9acac97fccf1e74b42b89c49f/library/src/scala/math/Ordering.scala#L206)
        if (x eq y) // error
          return 0

        ???
    }
}
