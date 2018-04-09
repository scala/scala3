package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.quoted.PickledQuotes

import scala.tasty.trees

private[dotc] class TastyContext(val ctx: Context) extends scala.tasty.Context {
  def owner: trees.Definition = Definition(ctx.owner)(ctx)

  def toTasty[T](expr: quoted.Expr[T]): trees.Term =
    internal.Term(PickledQuotes.quotedExprToTree(expr)(ctx))

  def toTasty[T](tpe: quoted.Type[T]): trees.TypeTree =
    internal.TypeTree(PickledQuotes.quotedTypeToTree(tpe)(ctx))

  def toolbox: scala.runtime.tasty.Toolbox = Toolbox
}
