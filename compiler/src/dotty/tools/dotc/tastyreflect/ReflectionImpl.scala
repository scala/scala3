package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core._
import dotty.tools.dotc.util.{SourcePosition, Spans}

object ReflectionImpl {

  def apply(rootContext: Contexts.Context): scala.tasty.Reflection =
    apply(rootContext, SourcePosition(rootContext.source, Spans.NoSpan))

  def apply(rootContext: Contexts.Context, rootPosition: SourcePosition): scala.tasty.Reflection =
    new ReflectionImpl(new KernelImpl(rootContext, rootPosition))

  def showTree(tree: tpd.Tree)(implicit ctx: Contexts.Context): String = {
    val refl = new ReflectionImpl(new KernelImpl(ctx, tree.sourcePos))
    new refl.SourceCodePrinter().showTree(tree)
  }

  private class ReflectionImpl(val kernel: KernelImpl) extends scala.tasty.Reflection

}
