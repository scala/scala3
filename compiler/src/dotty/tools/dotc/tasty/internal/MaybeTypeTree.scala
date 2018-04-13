package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.typetrees

object MaybeTypeTree {

  def apply(arg: tpd.Tree)(implicit ctx: Context): typetrees.MaybeTypeTree = arg match {
    case arg: Trees.TypeBoundsTree[_] => TypeBoundsTree(arg)
    case _ => TypeTree(arg)
  }

}
