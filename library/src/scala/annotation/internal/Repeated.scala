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

/** An annotation produced by desugaring to indicate that a
 *  sequence is a repeated parameter. I.e.
 *
 *      T*  is expanded by Desugar to    Seq[T] @Repeated
 */
final class Repeated() extends Annotation
