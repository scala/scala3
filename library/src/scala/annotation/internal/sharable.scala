/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package scala.annotation.internal

import scala.annotation.Annotation

/** An annotation indicating to `-Ycheck:reentrant` that a class or val can be safely shared.
 *
 *  @see scala.annotation.internal.unshared
 */
class sharable extends Annotation
