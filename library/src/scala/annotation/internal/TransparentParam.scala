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

/** An annotation produced by Namer to indicate an inline parameter */
final class InlineParam() extends Annotation
