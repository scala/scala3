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

/** The class associated with a `BodyAnnotation`, which indicates
 *  an inline method's right hand side
 */
final class Body() extends Annotation
