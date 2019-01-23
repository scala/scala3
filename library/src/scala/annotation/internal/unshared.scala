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

/** An annotation indicating to `-Ycheck:reentrant` that an object will not be accessed from multiple threads.
 *
 *  @see scala.annotation.internal.sharable
 */
class unshared extends Annotation
