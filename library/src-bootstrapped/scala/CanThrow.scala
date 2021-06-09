package scala
import language.experimental.erasedDefinitions
import annotation.{implicitNotFound, experimental}

/** An ability class that allows to throw exception `E`. When used with the
 *  experimental.saferExceptions feature, a `throw Ex()` expression will require
 *  a given of class `CanThrow[Ex]` to be available.
 */
@experimental
@implicitNotFound("The ability to throw exception ${E} is missing.\nThe ability can be provided by one of the following:\n - A using clause `(using CanThrow[${E}])`\n - A `canThrow` clause in a result type such as `X canThrow ${E}`\n - an enclosing `try` that catches ${E}")
erased class CanThrow[-E <: Exception]

/** A helper type to allow syntax like
 *
 *    def f(): T canThrow Ex
 */
@experimental
infix type canThrow[R, +E <: Exception] = CanThrow[E] ?=> R

@experimental
object unsafeExceptions:
  given canThrowAny: CanThrow[Exception] = ???
