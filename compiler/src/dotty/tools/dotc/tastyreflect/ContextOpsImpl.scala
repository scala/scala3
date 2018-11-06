package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.util.{Positions, SourcePosition}

trait ContextOpsImpl extends scala.tasty.reflect.ContextOps with ReflectionCoreImpl {

  val rootContext: Context

  def ContextDeco(ctx: Context): ContextAPI = new ContextAPI {
    def owner: Symbol = ctx.owner

    def source: java.nio.file.Path = ctx.compilationUnit.source.file.jpath
  }

  def rootPosition: SourcePosition = SourcePosition(rootContext.source, Positions.NoPosition)

}
