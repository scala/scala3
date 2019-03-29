package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core._
import dotty.tools.dotc.util.{SourcePosition, Spans}

object ReflectionImpl {

  def apply(rootContext: Contexts.Context): scala.tasty.Reflection =
    apply(rootContext, SourcePosition(rootContext.source, Spans.NoSpan))

  def apply(rootContext: Contexts.Context, rootPosition: SourcePosition): scala.tasty.Reflection =
    new scala.tasty.Reflection(new KernelImpl(rootContext, rootPosition))

  def showTree(tree: tpd.Tree)(implicit ctx: Contexts.Context): String = {
    val refl = new scala.tasty.Reflection(new KernelImpl(ctx, tree.sourcePos))
    val reflCtx = ctx.asInstanceOf[refl.Context]
    val reflTree = tree.asInstanceOf[refl.Tree]
    new refl.SourceCodePrinter().showTree(reflTree)(reflCtx)
  }

}
