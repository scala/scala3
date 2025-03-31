package scala.runtime

import language.experimental.captureChecking

import annotation.experimental

/** A helper type to allow syntax like
 *
 *    def f(): T throws Ex1 | Ex2
 *
 *  Used in desugar.throws.
 */
@experimental
infix type $throws[R, +E <: Exception] = CanThrow[E] ?=> R
