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

package dotty.runtime

/** Replaces the `scala.App` class which relies on `DelayedInit` functionality,
  * not supported by Dotty.
  */
class LegacyApp {
  def main(args: Array[String]): Unit = ()
}
