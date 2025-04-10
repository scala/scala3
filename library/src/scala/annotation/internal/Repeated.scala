package scala.annotation.internal

import language.experimental.captureChecking

import scala.annotation.Annotation

/** An annotation produced by desugaring to indicate that a
 *  sequence is a repeated parameter. I.e.
 *
 *      T*  is expanded by Desugar to    Seq[T] @Repeated
 */
final class Repeated() extends Annotation
