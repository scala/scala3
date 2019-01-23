/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.util.{Positions, SourcePosition}

trait ContextOpsImpl extends scala.tasty.reflect.ContextOps with CoreImpl {

  val rootContext: Context

  def ContextDeco(ctx: Context): ContextAPI = new ContextAPI {
    def owner: Symbol = ctx.owner

    def source: java.nio.file.Path = ctx.compilationUnit.source.file.jpath
  }

  def rootPosition: SourcePosition = SourcePosition(rootContext.source, Positions.NoPosition)

}
