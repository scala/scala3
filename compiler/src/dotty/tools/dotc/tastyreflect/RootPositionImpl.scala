package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.util.{SourcePosition, Spans}

trait RootPositionImpl extends scala.tasty.reflect.RootPosition with ContextOpsImpl with CoreImpl {

  protected def withDefaultPos[T <: Tree](fn: Context => T)(implicit ctx: Context): T = {
    fn(ctx.withSource(rootPosition.source)).withSpan(rootPosition.span)
  }

}
