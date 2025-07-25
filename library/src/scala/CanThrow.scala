package scala
import language.experimental.erasedDefinitions
import annotation.{implicitNotFound, experimental, capability}

/** A capability class that allows to throw exception `E`. When used with the
 *  experimental.saferExceptions feature, a `throw Ex()` expression will require
 *  a given of class `CanThrow[Ex]` to be available.
 */
@experimental
@implicitNotFound("The capability to throw exception ${E} is missing.\nThe capability can be provided by one of the following:\n - Adding a using clause `(using CanThrow[${E}])` to the definition of the enclosing method\n - Adding `throws ${E}` clause after the result type of the enclosing method\n - Wrapping this piece of code with a `try` block that catches ${E}")
class CanThrow[-E <: Exception] extends caps.Control, compiletime.Erased

@experimental
object unsafeExceptions:
  inline given canThrowAny: CanThrow[Exception] = caps.unsafe.unsafeErasedValue

