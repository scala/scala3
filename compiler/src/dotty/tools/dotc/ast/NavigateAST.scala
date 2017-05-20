package dotty.tools.dotc
package ast

import core.Contexts.Context
import core.Decorators._
import util.Positions._
import Trees.{MemberDef, DefTree}

/** Utility functions to go from typed to untyped ASTs
 *  and to navigate in untyped ASTs
 */
object NavigateAST {

  /** The untyped tree corresponding to typed tree `tree` in the compilation
   *  unit specified by `ctx`
   */
  def toUntyped(tree: tpd.Tree)(implicit ctx: Context): untpd.Tree =
    untypedPath(tree, exactMatch = true) match {
      case (utree: untpd.Tree) :: _ =>
        utree
      case _ =>
        val loosePath = untypedPath(tree, exactMatch = false)
        throw new
          Error(i"""no untyped tree for $tree, pos = ${tree.pos}
                   |best matching path =\n$loosePath%\n====\n%
                   |path positions = ${loosePath.map(_.pos)}""")
    }

  /** The reverse path of untyped trees starting with a tree that closest matches
   *  `tree` and ending in the untyped tree at the root of the compilation unit
   *  specified by `ctx`.
   *  @param exactMatch    If `true`, the path must start with a node that exactly
   *                       matches `tree`, or `Nil` is returned.
   *                       If `false` the path might start with a node enclosing
   *                       the logical position of `tree`.
   *  Note: A complication concerns member definitions. ValDefs and DefDefs
   *  have after desugaring a position that spans just the name of the symbol being
   *  defined and nothing else. So we look instead for an untyped tree approximating the
   *  envelope of the definition, and declare success if we find another DefTree.
   */
  def untypedPath(tree: tpd.Tree, exactMatch: Boolean = false)(implicit ctx: Context): List[Positioned] =
    tree match {
      case tree: MemberDef[_] =>
        untypedPath(tree.pos) match {
          case path @ (last: DefTree[_]) :: _ => path
          case path if !exactMatch => path
          case _ => Nil
        }
      case _ =>
        untypedPath(tree.pos) match {
          case (path @ last :: _) if last.pos == tree.pos || !exactMatch => path
          case _ => Nil
        }
    }

  /** The reverse part of the untyped root of the compilation unit of `ctx` to
   *  position `pos`.
   */
  def untypedPath(pos: Position)(implicit ctx: Context): List[Positioned] =
    pathTo(pos, ctx.compilationUnit.untpdTree)

  /** The reverse path from node `from` to the node that closest encloses position `pos`,
   *  or `Nil` if no such path exists. If a non-empty path is returned it starts with
   *  the node closest enclosing `pos` and ends with `from`.
   */
  def pathTo(pos: Position, from: Positioned)(implicit ctx: Context): List[Positioned] = {
    def childPath(it: Iterator[Any], path: List[Positioned]): List[Positioned] = {
      while (it.hasNext) {
        val path1 = it.next match {
          case p: Positioned => singlePath(p, path)
          case xs: List[_] => childPath(xs.iterator, path)
          case _ => path
        }
        if (path1 ne path) return path1
      }
      path
    }
    def singlePath(p: Positioned, path: List[Positioned]): List[Positioned] =
      if (p.pos contains pos) childPath(p.productIterator, p :: path)
      else path
    singlePath(from, Nil)
  }

  /** The subtrees of `roots` that immediately precede `offset`, from outer to inner.
   *  Every returned subtree `t` satisfies the following condition
   *
   *    - t's position is a non-empty, non-synthetic interval
   *    - t's end position  <=  offset`  and
   *    - there is no other eligible tree t' which lies entirely between t's end position and `offset`.
   */
  def precedingTrees(offset: Int, roots: List[Positioned])(implicit ctx: Context): List[Positioned] = {
    def precedingPath(xs: List[Any], path: List[Positioned]): List[Positioned] = xs match {
      case (p: Positioned) :: xs1
      if p.pos.exists && p.pos.start < p.pos.end && p.pos.start <= offset =>
        val children = p.productIterator.toList.reverse
        if (!p.pos.isSynthetic && p.pos.end <= offset) precedingPath(children, p :: path)
        else precedingPath(children ::: xs1, path)
      case (ys: List[_]) :: xs1 =>
        precedingPath(ys.reverse ::: xs1, path)
      case _ :: xs1 =>
        precedingPath(xs1, path)
      case _ =>
        path
    }
    precedingPath(roots.reverse, Nil)
  }
}