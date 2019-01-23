/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package dotty.tools.dotc
package transform

import core._
import ast.Trees._
import Contexts._
import Symbols._
import annotation.tailrec

/** A Macrotransform that maintains the necessary infrastructore to support
 *  contxtual implicit searches (type-scope implicits are supported anyway).
 */
abstract class MacroTransformWithImplicits extends MacroTransform {
  import ast.tpd._

  override def allowsImplicitSearch: Boolean = true

  class ImplicitsTransformer extends Transformer {

    /** Transform statements, while maintaining import contexts and expression contexts
     *  in the same way as Typer does. The code addresses additional concerns:
     *   - be tail-recursive where possible
     *   - don't re-allocate trees where nothing has changed
     */
    override def transformStats(stats: List[Tree], exprOwner: Symbol)(implicit ctx: Context): List[Tree] = {

      @tailrec def traverse(curStats: List[Tree])(implicit ctx: Context): List[Tree] = {

        def recur(stats: List[Tree], changed: Tree, rest: List[Tree])(implicit ctx: Context): List[Tree] = {
          if (stats eq curStats) {
            val rest1 = transformStats(rest, exprOwner)
            changed match {
              case Thicket(trees) => trees ::: rest1
              case tree => tree :: rest1
            }
          }
          else stats.head :: recur(stats.tail, changed, rest)
        }

        curStats match {
          case stat :: rest =>
            val statCtx = stat match {
              case stat: DefTree => ctx
              case _ => ctx.exprContext(stat, exprOwner)
            }
            val restCtx = stat match {
              case stat: Import => ctx.importContext(stat, stat.symbol)
              case _ => ctx
            }
            val stat1 = transform(stat)(statCtx)
            if (stat1 ne stat) recur(stats, stat1, rest)(restCtx)
            else traverse(rest)(restCtx)
          case nil =>
            stats
        }
      }
      traverse(stats)
    }

    private def nestedScopeCtx(defs: List[Tree])(implicit ctx: Context): Context = {
      val nestedCtx = ctx.fresh.setNewScope
      defs foreach {
        case d: DefTree => nestedCtx.enter(d.symbol)
        case _ =>
      }
      nestedCtx
    }

    override def transform(tree: Tree)(implicit ctx: Context): Tree = {
      def localCtx = ctx.withOwner(tree.symbol)
      tree match {
        case tree: Block =>
          super.transform(tree)(nestedScopeCtx(tree.stats))
        case tree: DefDef =>
          implicit val ctx = localCtx
          cpy.DefDef(tree)(
            tree.name,
            transformSub(tree.tparams),
            tree.vparamss mapConserve (transformSub(_)),
            transform(tree.tpt),
            transform(tree.rhs)(nestedScopeCtx(tree.vparamss.flatten)))
        case _ =>
          super.transform(tree)
      }
    }
  }
}