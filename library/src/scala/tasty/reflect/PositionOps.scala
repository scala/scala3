/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package scala.tasty.reflect

trait PositionOps extends Core {

  trait PositionAPI {

    def exists: Boolean

    def sourceFile: java.nio.file.Path

    /** The start index in the source file */
    def start: Int

    /** The end index in the source file */
    def end: Int

    /** The start line in the source file */
    def startLine: Int

    /** The start column in the source file */
    def startColumn: Int

    /** The end line in the source file */
    def endLine: Int

    /** The end column in the source file */
    def endColumn: Int

  }
  implicit def PositionDeco(pos: Position): PositionAPI

}
