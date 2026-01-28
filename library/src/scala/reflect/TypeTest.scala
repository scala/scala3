package scala.reflect

import language.experimental.captureChecking

/** A `TypeTest[S, T]` contains the logic needed to know at runtime if a value of
 *  type `S` is an instance of `T`.
 *
 *  If a pattern match is performed on a term of type `s: S` that is uncheckable with `s.isInstanceOf[T]` and
 *  the pattern is one of the following forms:
 *    - `t: T`
 *    - `t @ X()` where `X.unapply` takes an argument of type `T`
 *  then a given instance of `TypeTest[S, T]` is summoned and used to perform the test.
 */
@scala.annotation.implicitNotFound(msg = "No TypeTest available for [${S}, ${T}]")
trait TypeTest[-S, T] extends Serializable:

  /** A `TypeTest[S, T]` can serve as an extractor that matches if and only if a value of type `S` is
   *  an instance of `T`.
   *
   *  The compiler tries to turn unchecked type tests in pattern matches into checked ones
   *  by wrapping a `(_: T)` type pattern as `tt(_: T)`, where `tt` is the `TypeTest[S, T]` instance.
   *  Type tests necessary before calling other extractors are treated similarly.
   *  `SomeExtractor(...)` is turned into `tt(SomeExtractor(...))` if `T` in `SomeExtractor.unapply(x: T)`
   *  is uncheckable, but we have an instance of `TypeTest[S, T]`.
   */
  def unapply(x: S): Option[x.type & T]

object TypeTest:

  /** Trivial type test that always succeeds. */
  def identity[T]: TypeTest[T, T] = Some(_)
