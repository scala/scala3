package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.core.Contexts
import dotty.tools.dotc.tastyreflect.FromSymbol.definitionFromSym
import dotty.tools.dotc.util.{Positions, SourcePosition}

trait ContextOpsImpl extends scala.tasty.reflect.ContextOps with TastyCoreImpl {

  val rootContext: Contexts.Context

  def ContextDeco(ctx: Context): ContextAPI = new ContextAPI {
    def owner: Definition = definitionFromSym(ctx.owner)(ctx)

    def source: java.nio.file.Path = ctx.compilationUnit.source.file.jpath
  }

  def rootPosition: SourcePosition = SourcePosition(rootContext.source, Positions.NoPosition)

}
