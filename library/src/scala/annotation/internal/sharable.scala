package scala.annotation.internal

import language.experimental.captureChecking

import scala.annotation.Annotation

/** An annotation indicating to `-Ycheck:reentrant` that a class or val can be safely shared.
 *
 *  @see scala.annotation.internal.unshared
 */
class sharable extends Annotation
