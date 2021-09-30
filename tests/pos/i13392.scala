package scala
import language.experimental.erasedDefinitions
import annotation.{implicitNotFound, experimental}

@experimental
@implicitNotFound("The capability to throw exception ${E} is missing.\nThe capability can be provided by one of the following:\n - A using clause `(using CanThrow[${E}])`\n - A `throws` clause in a result type such as `X throws ${E}`\n - an enclosing `try` that catches ${E}")
erased class CanThrow[-E <: Exception]

@experimental
object unsafeExceptions:
  given canThrowAny: CanThrow[Exception] = ???
