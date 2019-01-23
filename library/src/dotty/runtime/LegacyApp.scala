/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package dotty.runtime

/** Replaces the `scala.App` class which relies on `DelayedInit` functionality,
  * not supported by Dotty.
  */
class LegacyApp {
  def main(args: Array[String]): Unit = ()
}
