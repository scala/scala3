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

package dotty.tools.dotc.util

/** Defines a key type with which to tag properties, such as attachments
 *  or context properties
 */
object Property {

  /** The class of keys for properties of type V */
  class Key[+V]

  /**
   * The class of keys for sticky properties of type V
   *
   * Sticky properties are properties that should be copied over when their container
   * is copied.
   */
  class StickyKey[+V] extends Key[V]
}
