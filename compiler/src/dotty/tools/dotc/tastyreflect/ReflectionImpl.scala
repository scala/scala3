package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.core._
import dotty.tools.dotc.util.{SourcePosition, Spans}

class ReflectionImpl private (ctx: Contexts.Context, pos: SourcePosition)
    extends scala.tasty.Reflection
    with CoreImpl
    with CaseDefOpsImpl
    with ConstantOpsImpl
    with ContextOpsImpl
    with CommentOpsImpl
    with FlagsOpsImpl
    with IdOpsImpl
    with ImportSelectorOpsImpl
    with QuotedOpsImpl
    with PatternOpsImpl
    with PositionOpsImpl
    with PrintersImpl
    with RootPositionImpl
    with SettingsOpsImpl
    with SignatureOpsImpl
    with StandardDefinitions
    with SymbolOpsImpl
    with TreeOpsImpl
    with TypeOrBoundsTreesOpsImpl
    with TypeOrBoundsOpsImpl {

  val kernel: KernelImpl = new KernelImpl(ctx, pos)

}

object ReflectionImpl {

  def apply(rootContext: Contexts.Context): ReflectionImpl =
    apply(rootContext, SourcePosition(rootContext.source, Spans.NoSpan))

  def apply(rootContext: Contexts.Context, rootPosition: SourcePosition): ReflectionImpl =
    new ReflectionImpl(rootContext, rootPosition)

}
