package scala.util

import language.experimental.captureChecking

/** A special class used to implement negation in implicit search.
 *
 *  Consider the problem of using implicit `i1` for a query type `D` if an implicit
 *  for some other class `C` is available, and using an implicit `i2` if no implicit
 *  value of type `C` is available. If we do not want to prioritize `i1` and `i2` by
 *  putting them in different traits we can instead define the following:
 *
 *     given i1: D(using ev: C) = ...
 *     given i2: D(using ev: NotGiven[C]) = ...
 *
 *  `NotGiven` is treated specially in implicit search, similar to the way logical negation
 *  is treated in Prolog: The implicit search for `NotGiven[C]` succeeds if and only if the implicit
 *  search for `C` fails.
 *
 *  In Scala 2 this form of negation can be simulated by setting up a conditional
 *  ambiguous implicit and an unconditional fallback, the way it is done with the
 *  `default`, `amb1` and `amb2` methods below. Due to the way these two methods are
 *  defined, `NotGiven` is also usable from Scala 2.
 *
 *  In Dotty, ambiguity is a global error, and therefore cannot be used to implement negation.
 *  Instead, `NotGiven` is treated natively in implicit search.
 *
 *  @tparam T the type for which no implicit instance should be available
 */
final class NotGiven[+T] private ()

trait LowPriorityNotGiven {

  /** A fallback method used to emulate negation in Scala 2. */
  given default[T]: NotGiven[T] = NotGiven.value
}
object NotGiven extends LowPriorityNotGiven {

  private val cachedValue = new NotGiven[Nothing]()

  /** A value of type `NotGiven` to signal a successful search for `NotGiven[C]` (i.e. a failing
   *  search for `C`). A reference to this value will be explicitly constructed by Dotty's
   *  implicit search algorithm
   */
  def value: NotGiven[Nothing] = cachedValue

  /** One of two ambiguous methods used to emulate negation in Scala 2. */
  given amb1[T](using ev: T): NotGiven[T] = ???

  /** One of two ambiguous methods used to emulate negation in Scala 2. */
  given amb2[T](using ev: T): NotGiven[T] = ???
}
