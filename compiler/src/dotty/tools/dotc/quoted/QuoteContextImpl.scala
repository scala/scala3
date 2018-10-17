package dotty.tools.dotc.quoted

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.quoted.PickledQuotes
import dotty.tools.dotc.tastyreflect.ReflectionImpl

import scala.quoted.{Expr, Type}
import scala.runtime.quoted.Unpickler.Pickled

class StagingImpl(implicit ctx: Context) extends scala.quoted.StagingContext {

  def unpickleExpr[T](repr: Pickled, args: Seq[Any]): Expr[T] =
    new scala.quoted.Exprs.TastyTreeExpr(PickledQuotes.unpickleExpr(repr, args), PickledQuotes.contextId).asInstanceOf[Expr[T]]

  def unpickleType[T](repr: Pickled, args: Seq[Any]): Type[T] =
    new scala.quoted.Types.TreeType(PickledQuotes.unpickleType(repr, args)).asInstanceOf[Type[T]]

  def show[T](expr: Expr[T]): String = {
    val tree = PickledQuotes.quotedExprToTree(expr)
    // TODO freshen names
    val tree1 =
     if (ctx.settings.YshowRawQuoteTrees.value) tree else (new TreeCleaner).transform(tree)
    new ReflectionImpl(ctx).showSourceCode.showTree(tree1)
  }

  def show[T](tpe: Type[T]): String = {
    val tree = PickledQuotes.quotedTypeToTree(tpe)
    // TODO freshen names
    val tree1 = if (ctx.settings.YshowRawQuoteTrees.value) tree else (new TreeCleaner).transform(tree)
    new ReflectionImpl(ctx).showSourceCode.showTypeOrBoundsTree(tree1)
  }

  val reflection: scala.tasty.Reflection = new ReflectionImpl(ctx)

}
