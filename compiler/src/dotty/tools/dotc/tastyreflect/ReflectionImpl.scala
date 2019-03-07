package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.core._
import dotty.tools.dotc.util.{SourcePosition, Spans}

object ReflectionImpl {

  def apply(rootContext: Contexts.Context): scala.tasty.Reflection { val kernel: KernelImpl } =
    apply(rootContext, SourcePosition(rootContext.source, Spans.NoSpan))

  def apply(rootContext: Contexts.Context, rootPosition: SourcePosition): scala.tasty.Reflection { val kernel: KernelImpl } = {
    class ReflectionImpl(val kernel: KernelImpl) extends scala.tasty.Reflection
    new ReflectionImpl(new KernelImpl(rootContext, rootPosition))
  }

}
