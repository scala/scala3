package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.core._
import dotty.tools.dotc.util.{SourcePosition, Spans}

class ReflectionImpl private (ctx: Contexts.Context, pos: SourcePosition)
    extends scala.tasty.Reflection
    with CoreImpl
    with CommentOpsImpl
    with SymbolOpsImpl {

  val kernel: KernelImpl = new KernelImpl(ctx, pos)

}

object ReflectionImpl {

  def apply(rootContext: Contexts.Context): ReflectionImpl =
    apply(rootContext, SourcePosition(rootContext.source, Spans.NoSpan))

  def apply(rootContext: Contexts.Context, rootPosition: SourcePosition): ReflectionImpl =
    new ReflectionImpl(rootContext, rootPosition)

}
