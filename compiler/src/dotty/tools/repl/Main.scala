/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package dotty.tools.repl

/** Main entry point to the REPL */
object Main {
  def main(args: Array[String]): Unit =
    new ReplDriver(args).runUntilQuit()
}
