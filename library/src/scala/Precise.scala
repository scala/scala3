package scala

import language.experimental.captureChecking

import annotation.experimental
import language.experimental.erasedDefinitions

/** A type class-like trait intended as a context bound for type variables.
 *  If we have `[X: Precise]`, instances of the type variable `X` are inferred
 *  in precise mode. This means that singleton types and union types are not
 *  widened.
 */
@experimental trait Precise extends compiletime.Erased:
  type Self
