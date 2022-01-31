package dotty.tools
package dotc
package transform

import core._
import Contexts._, Symbols._, Decorators._
import MegaPhase._
import ast.Trees._

/** Rewrite `{ stats; expr}.f(args)` to `{ stats; expr.f(args) }` and
 *  `{ stats; expr }(args)` to `{ stats; expr(args) }` before proceeding,
 *  but leave closures alone. This is necessary to be able to
 *  collapse applies of IFTs (this is done in Erasure).
 */
class LetOverApply extends MiniPhase:
  import ast.tpd._

  override def phaseName: String = LetOverApply.name

  override def description: String = LetOverApply.description

  override def transformApply(tree: Apply)(using Context): Tree =
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

object LetOverApply:
  val name: String = "letOverApply"
  val description: String = "lift blocks from receivers of applications"
