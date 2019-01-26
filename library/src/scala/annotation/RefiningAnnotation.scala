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

package scala.annotation

/** A base trait for annotations that yield proper subtypes of the types they annotate.
 *  Refining annotations are more "sticky" than normal ones. They are conceptually kept
 *  around when normal refinements would also not be stripped away.
 */
trait RefiningAnnotation extends StaticAnnotation
