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

package dotty.tools.dotc.tastyreflect

trait PositionOpsImpl extends scala.tasty.reflect.PositionOps with CoreImpl {

  def PositionDeco(pos: Position): PositionAPI = new PositionAPI {
    def start: Int = pos.start
    def end: Int = pos.end

    def exists: Boolean = pos.exists

    def sourceFile: java.nio.file.Path = pos.source.file.jpath

    def startLine: Int = pos.startLine
    def endLine: Int = pos.endLine

    def startColumn: Int = pos.startColumn
    def endColumn: Int = pos.endColumn
  }
}
