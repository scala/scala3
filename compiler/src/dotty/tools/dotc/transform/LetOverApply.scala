package dotty.tools
package dotc
package transform

import core._
import MegaPhase._
import Contexts.Context
import Symbols._, Decorators._, Types._
import ast.Trees._
import dotty.tools.dotc.ast.tpd


import scala.collection.immutable.::


/** Rewrite `{ stats; expr}.f(args) }` to `{ stats; expr.f(args) }` before
 *  proceeding, but leave closures alone. This is necessary to be able to
 *  collapse applies of IFTs.
 */
class LetOverApply extends MiniPhase:
  import ast.tpd._

  override def phaseName: String = "letOverApply"

  override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree =
    tree.fun match
      case Select(blk @ Block(stats, expr), name) if !expr.isInstanceOf[Closure] =>
        cpy.Block(blk)(stats,
          cpy.Apply(tree)(
            cpy.Select(tree.fun)(expr, name), tree.args))
      case Block(stats, expr) =>
        cpy.Block(tree.fun)(stats,
          cpy.Apply(tree)(expr, tree.args))
      case _ =>
        tree

end LetOverApply
