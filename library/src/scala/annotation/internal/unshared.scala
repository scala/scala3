/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.annotation.internal

import scala.annotation.Annotation

/** An annotation indicating to `-Ycheck:reentrant` that an object will not be accessed from multiple threads.
 *
 *  @see scala.annotation.internal.sharable
 */
class unshared extends Annotation
